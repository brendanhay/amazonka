{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateInstanceMetadataOptionsState
  = LTIMOSApplied
  | LTIMOSPending
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

instance FromText LaunchTemplateInstanceMetadataOptionsState where
  parser =
    takeLowerText >>= \case
      "applied" -> pure LTIMOSApplied
      "pending" -> pure LTIMOSPending
      e ->
        fromTextError $
          "Failure parsing LaunchTemplateInstanceMetadataOptionsState from value: '" <> e
            <> "'. Accepted values: applied, pending"

instance ToText LaunchTemplateInstanceMetadataOptionsState where
  toText = \case
    LTIMOSApplied -> "applied"
    LTIMOSPending -> "pending"

instance Hashable LaunchTemplateInstanceMetadataOptionsState

instance NFData LaunchTemplateInstanceMetadataOptionsState

instance ToByteString LaunchTemplateInstanceMetadataOptionsState

instance ToQuery LaunchTemplateInstanceMetadataOptionsState

instance ToHeader LaunchTemplateInstanceMetadataOptionsState

instance FromXML LaunchTemplateInstanceMetadataOptionsState where
  parseXML = parseXMLText "LaunchTemplateInstanceMetadataOptionsState"
