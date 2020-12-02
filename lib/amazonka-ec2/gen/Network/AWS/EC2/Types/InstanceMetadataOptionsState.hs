{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceMetadataOptionsState
  = IMOSApplied
  | IMOSPending
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

instance FromText InstanceMetadataOptionsState where
  parser =
    takeLowerText >>= \case
      "applied" -> pure IMOSApplied
      "pending" -> pure IMOSPending
      e ->
        fromTextError $
          "Failure parsing InstanceMetadataOptionsState from value: '" <> e
            <> "'. Accepted values: applied, pending"

instance ToText InstanceMetadataOptionsState where
  toText = \case
    IMOSApplied -> "applied"
    IMOSPending -> "pending"

instance Hashable InstanceMetadataOptionsState

instance NFData InstanceMetadataOptionsState

instance ToByteString InstanceMetadataOptionsState

instance ToQuery InstanceMetadataOptionsState

instance ToHeader InstanceMetadataOptionsState

instance FromXML InstanceMetadataOptionsState where
  parseXML = parseXMLText "InstanceMetadataOptionsState"
