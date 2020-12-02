{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIKeySourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.APIKeySourceType where

import Network.AWS.Prelude

data APIKeySourceType
  = Authorizer
  | Header
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText APIKeySourceType where
  parser =
    takeLowerText >>= \case
      "authorizer" -> pure Authorizer
      "header" -> pure Header
      e ->
        fromTextError $
          "Failure parsing APIKeySourceType from value: '" <> e
            <> "'. Accepted values: authorizer, header"

instance ToText APIKeySourceType where
  toText = \case
    Authorizer -> "AUTHORIZER"
    Header -> "HEADER"

instance Hashable APIKeySourceType

instance NFData APIKeySourceType

instance ToByteString APIKeySourceType

instance ToQuery APIKeySourceType

instance ToHeader APIKeySourceType

instance ToJSON APIKeySourceType where
  toJSON = toJSONText

instance FromJSON APIKeySourceType where
  parseJSON = parseJSONText "APIKeySourceType"
