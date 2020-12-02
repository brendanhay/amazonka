{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingType where

import Network.AWS.Prelude

data OfferingType = Recurring
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

instance FromText OfferingType where
  parser =
    takeLowerText >>= \case
      "recurring" -> pure Recurring
      e ->
        fromTextError $
          "Failure parsing OfferingType from value: '" <> e
            <> "'. Accepted values: recurring"

instance ToText OfferingType where
  toText = \case
    Recurring -> "RECURRING"

instance Hashable OfferingType

instance NFData OfferingType

instance ToByteString OfferingType

instance ToQuery OfferingType

instance ToHeader OfferingType

instance FromJSON OfferingType where
  parseJSON = parseJSONText "OfferingType"
