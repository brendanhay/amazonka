{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Scope where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data Scope
  = SAvailabilityZone
  | SRegion
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

instance FromText Scope where
  parser =
    takeLowerText >>= \case
      "availability zone" -> pure SAvailabilityZone
      "region" -> pure SRegion
      e ->
        fromTextError $
          "Failure parsing Scope from value: '" <> e
            <> "'. Accepted values: availability zone, region"

instance ToText Scope where
  toText = \case
    SAvailabilityZone -> "Availability Zone"
    SRegion -> "Region"

instance Hashable Scope

instance NFData Scope

instance ToByteString Scope

instance ToQuery Scope

instance ToHeader Scope

instance FromXML Scope where
  parseXML = parseXMLText "Scope"
