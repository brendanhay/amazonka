{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.ShapeRef
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Types where

import Data.Aeson.Decoding.Tokens (Tokens)
import Data.Aeson.Decoding.Tokens.Direct (Parser, text)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MemberName = MemberName {unMemberName :: Text}
  deriving (Eq, Show, Generic)

memberName :: Parser Tokens k e MemberName
memberName = MemberName <$> text

newtype ShapeName = ShapeName {unShapeName :: Text}
  deriving (Eq, Show, Generic)

shapeName :: Parser Tokens k e ShapeName
shapeName = ShapeName <$> text
