{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Types
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Types where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    field,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MemberName = MemberName {unMemberName :: Text}
  deriving stock (Eq, Show, Generic)

memberName :: Parser Tokens k e MemberName
memberName = MemberName <$> text

newtype ShapeName = ShapeName {unShapeName :: Text}
  deriving stock (Eq, Show, Generic)

shapeName :: Parser Tokens k e ShapeName
shapeName = ShapeName <$> text

$( passthroughBareB
     [d|
       data XmlNamespace = XmlNamespace {uri :: Text, prefix :: Maybe Text}
         deriving stock (Eq, Show, Generic)
       |]
 )

xmlNamespace :: Parser Tokens k e XmlNamespace
xmlNamespace =
  record
    XmlNamespace
      { uri = field "uri" text,
        prefix = optional $ field "prefix" text
      }
