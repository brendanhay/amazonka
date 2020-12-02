{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode where

import Network.AWS.Prelude

-- | Global Configuration Output Locking Mode
data GlobalConfigurationOutputLockingMode
  = EpochLocking
  | PipelineLocking
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

instance FromText GlobalConfigurationOutputLockingMode where
  parser =
    takeLowerText >>= \case
      "epoch_locking" -> pure EpochLocking
      "pipeline_locking" -> pure PipelineLocking
      e ->
        fromTextError $
          "Failure parsing GlobalConfigurationOutputLockingMode from value: '" <> e
            <> "'. Accepted values: epoch_locking, pipeline_locking"

instance ToText GlobalConfigurationOutputLockingMode where
  toText = \case
    EpochLocking -> "EPOCH_LOCKING"
    PipelineLocking -> "PIPELINE_LOCKING"

instance Hashable GlobalConfigurationOutputLockingMode

instance NFData GlobalConfigurationOutputLockingMode

instance ToByteString GlobalConfigurationOutputLockingMode

instance ToQuery GlobalConfigurationOutputLockingMode

instance ToHeader GlobalConfigurationOutputLockingMode

instance ToJSON GlobalConfigurationOutputLockingMode where
  toJSON = toJSONText

instance FromJSON GlobalConfigurationOutputLockingMode where
  parseJSON = parseJSONText "GlobalConfigurationOutputLockingMode"
