{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppNetworkAccessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppNetworkAccessType where

import Network.AWS.Prelude

data AppNetworkAccessType
  = PublicInternetOnly
  | VPCOnly
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

instance FromText AppNetworkAccessType where
  parser =
    takeLowerText >>= \case
      "publicinternetonly" -> pure PublicInternetOnly
      "vpconly" -> pure VPCOnly
      e ->
        fromTextError $
          "Failure parsing AppNetworkAccessType from value: '" <> e
            <> "'. Accepted values: publicinternetonly, vpconly"

instance ToText AppNetworkAccessType where
  toText = \case
    PublicInternetOnly -> "PublicInternetOnly"
    VPCOnly -> "VpcOnly"

instance Hashable AppNetworkAccessType

instance NFData AppNetworkAccessType

instance ToByteString AppNetworkAccessType

instance ToQuery AppNetworkAccessType

instance ToHeader AppNetworkAccessType

instance ToJSON AppNetworkAccessType where
  toJSON = toJSONText

instance FromJSON AppNetworkAccessType where
  parseJSON = parseJSONText "AppNetworkAccessType"
