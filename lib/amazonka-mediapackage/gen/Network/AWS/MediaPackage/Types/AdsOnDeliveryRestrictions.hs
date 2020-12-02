{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions where

import Network.AWS.Prelude

-- | This setting allows the delivery restriction flags on SCTE-35 segmentation descriptors to
--
-- determine whether a message signals an ad.  Choosing "NONE" means no SCTE-35 messages become
-- ads.  Choosing "RESTRICTED" means SCTE-35 messages of the types specified in AdTriggers that
-- contain delivery restrictions will be treated as ads.  Choosing "UNRESTRICTED" means SCTE-35
-- messages of the types specified in AdTriggers that do not contain delivery restrictions will
-- be treated as ads.  Choosing "BOTH" means all SCTE-35 messages of the types specified in
-- AdTriggers will be treated as ads.  Note that Splice Insert messages do not have these flags
-- and are always treated as ads if specified in AdTriggers.
data AdsOnDeliveryRestrictions
  = Both
  | None
  | Restricted
  | Unrestricted
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

instance FromText AdsOnDeliveryRestrictions where
  parser =
    takeLowerText >>= \case
      "both" -> pure Both
      "none" -> pure None
      "restricted" -> pure Restricted
      "unrestricted" -> pure Unrestricted
      e ->
        fromTextError $
          "Failure parsing AdsOnDeliveryRestrictions from value: '" <> e
            <> "'. Accepted values: both, none, restricted, unrestricted"

instance ToText AdsOnDeliveryRestrictions where
  toText = \case
    Both -> "BOTH"
    None -> "NONE"
    Restricted -> "RESTRICTED"
    Unrestricted -> "UNRESTRICTED"

instance Hashable AdsOnDeliveryRestrictions

instance NFData AdsOnDeliveryRestrictions

instance ToByteString AdsOnDeliveryRestrictions

instance ToQuery AdsOnDeliveryRestrictions

instance ToHeader AdsOnDeliveryRestrictions

instance ToJSON AdsOnDeliveryRestrictions where
  toJSON = toJSONText

instance FromJSON AdsOnDeliveryRestrictions where
  parseJSON = parseJSONText "AdsOnDeliveryRestrictions"
