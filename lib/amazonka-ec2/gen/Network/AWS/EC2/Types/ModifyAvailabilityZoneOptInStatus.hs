{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyAvailabilityZoneOptInStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ModifyAvailabilityZoneOptInStatus
  = NotOptedIn
  | OptedIn
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

instance FromText ModifyAvailabilityZoneOptInStatus where
  parser =
    takeLowerText >>= \case
      "not-opted-in" -> pure NotOptedIn
      "opted-in" -> pure OptedIn
      e ->
        fromTextError $
          "Failure parsing ModifyAvailabilityZoneOptInStatus from value: '" <> e
            <> "'. Accepted values: not-opted-in, opted-in"

instance ToText ModifyAvailabilityZoneOptInStatus where
  toText = \case
    NotOptedIn -> "not-opted-in"
    OptedIn -> "opted-in"

instance Hashable ModifyAvailabilityZoneOptInStatus

instance NFData ModifyAvailabilityZoneOptInStatus

instance ToByteString ModifyAvailabilityZoneOptInStatus

instance ToQuery ModifyAvailabilityZoneOptInStatus

instance ToHeader ModifyAvailabilityZoneOptInStatus
