{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DirectInternetAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DirectInternetAccess where

import Network.AWS.Prelude

data DirectInternetAccess
  = DIADisabled
  | DIAEnabled
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

instance FromText DirectInternetAccess where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DIADisabled
      "enabled" -> pure DIAEnabled
      e ->
        fromTextError $
          "Failure parsing DirectInternetAccess from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DirectInternetAccess where
  toText = \case
    DIADisabled -> "Disabled"
    DIAEnabled -> "Enabled"

instance Hashable DirectInternetAccess

instance NFData DirectInternetAccess

instance ToByteString DirectInternetAccess

instance ToQuery DirectInternetAccess

instance ToHeader DirectInternetAccess

instance ToJSON DirectInternetAccess where
  toJSON = toJSONText

instance FromJSON DirectInternetAccess where
  parseJSON = parseJSONText "DirectInternetAccess"
