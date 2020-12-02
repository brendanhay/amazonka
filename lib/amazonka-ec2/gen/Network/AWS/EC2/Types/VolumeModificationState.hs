{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeModificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeModificationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeModificationState
  = Completed
  | Failed
  | Modifying
  | Optimizing
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

instance FromText VolumeModificationState where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "modifying" -> pure Modifying
      "optimizing" -> pure Optimizing
      e ->
        fromTextError $
          "Failure parsing VolumeModificationState from value: '" <> e
            <> "'. Accepted values: completed, failed, modifying, optimizing"

instance ToText VolumeModificationState where
  toText = \case
    Completed -> "completed"
    Failed -> "failed"
    Modifying -> "modifying"
    Optimizing -> "optimizing"

instance Hashable VolumeModificationState

instance NFData VolumeModificationState

instance ToByteString VolumeModificationState

instance ToQuery VolumeModificationState

instance ToHeader VolumeModificationState

instance FromXML VolumeModificationState where
  parseXML = parseXMLText "VolumeModificationState"
