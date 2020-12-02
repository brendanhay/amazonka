{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ApplianceModeSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ApplianceModeSupportValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ApplianceModeSupportValue
  = AMSVDisable
  | AMSVEnable
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

instance FromText ApplianceModeSupportValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure AMSVDisable
      "enable" -> pure AMSVEnable
      e ->
        fromTextError $
          "Failure parsing ApplianceModeSupportValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText ApplianceModeSupportValue where
  toText = \case
    AMSVDisable -> "disable"
    AMSVEnable -> "enable"

instance Hashable ApplianceModeSupportValue

instance NFData ApplianceModeSupportValue

instance ToByteString ApplianceModeSupportValue

instance ToQuery ApplianceModeSupportValue

instance ToHeader ApplianceModeSupportValue

instance FromXML ApplianceModeSupportValue where
  parseXML = parseXMLText "ApplianceModeSupportValue"
