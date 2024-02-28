{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Waiters
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Waiters where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Waiters.Waiter (Waiter)
import Botocore.Waiters.Waiter qualified as Waiter
import Data.Aeson.Decoding.Tokens (Tokens)
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    field,
    int,
    map,
    record,
  )
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

$( passthroughBareB
     [d|
       data Waiters = Waiters
         { version :: Int,
           waiters :: Map Text Waiter
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Waiters
parse =
  record
    Waiters
      { version = field "version" int,
        waiters = field "waiters" $ map Waiter.parse
      }
