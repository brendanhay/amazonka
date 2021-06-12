{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
  ( StackDriftDetectionStatus
      ( ..,
        StackDriftDetectionStatus_DETECTION_COMPLETE,
        StackDriftDetectionStatus_DETECTION_FAILED,
        StackDriftDetectionStatus_DETECTION_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackDriftDetectionStatus = StackDriftDetectionStatus'
  { fromStackDriftDetectionStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern StackDriftDetectionStatus_DETECTION_COMPLETE :: StackDriftDetectionStatus
pattern StackDriftDetectionStatus_DETECTION_COMPLETE = StackDriftDetectionStatus' "DETECTION_COMPLETE"

pattern StackDriftDetectionStatus_DETECTION_FAILED :: StackDriftDetectionStatus
pattern StackDriftDetectionStatus_DETECTION_FAILED = StackDriftDetectionStatus' "DETECTION_FAILED"

pattern StackDriftDetectionStatus_DETECTION_IN_PROGRESS :: StackDriftDetectionStatus
pattern StackDriftDetectionStatus_DETECTION_IN_PROGRESS = StackDriftDetectionStatus' "DETECTION_IN_PROGRESS"

{-# COMPLETE
  StackDriftDetectionStatus_DETECTION_COMPLETE,
  StackDriftDetectionStatus_DETECTION_FAILED,
  StackDriftDetectionStatus_DETECTION_IN_PROGRESS,
  StackDriftDetectionStatus'
  #-}
