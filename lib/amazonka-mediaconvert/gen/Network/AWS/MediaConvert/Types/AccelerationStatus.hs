{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationStatus where

import Network.AWS.Prelude

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
data AccelerationStatus
  = Accelerated
  | InProgress
  | NotAccelerated
  | NotApplicable
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

instance FromText AccelerationStatus where
  parser =
    takeLowerText >>= \case
      "accelerated" -> pure Accelerated
      "in_progress" -> pure InProgress
      "not_accelerated" -> pure NotAccelerated
      "not_applicable" -> pure NotApplicable
      e ->
        fromTextError $
          "Failure parsing AccelerationStatus from value: '" <> e
            <> "'. Accepted values: accelerated, in_progress, not_accelerated, not_applicable"

instance ToText AccelerationStatus where
  toText = \case
    Accelerated -> "ACCELERATED"
    InProgress -> "IN_PROGRESS"
    NotAccelerated -> "NOT_ACCELERATED"
    NotApplicable -> "NOT_APPLICABLE"

instance Hashable AccelerationStatus

instance NFData AccelerationStatus

instance ToByteString AccelerationStatus

instance ToQuery AccelerationStatus

instance ToHeader AccelerationStatus

instance FromJSON AccelerationStatus where
  parseJSON = parseJSONText "AccelerationStatus"
