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
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
  ( StackSetDriftDetectionStatus
      ( ..,
        StackSetDriftDetectionStatus_COMPLETED,
        StackSetDriftDetectionStatus_FAILED,
        StackSetDriftDetectionStatus_IN_PROGRESS,
        StackSetDriftDetectionStatus_PARTIAL_SUCCESS,
        StackSetDriftDetectionStatus_STOPPED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StackSetDriftDetectionStatus = StackSetDriftDetectionStatus'
  { fromStackSetDriftDetectionStatus ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern StackSetDriftDetectionStatus_COMPLETED :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatus_COMPLETED = StackSetDriftDetectionStatus' "COMPLETED"

pattern StackSetDriftDetectionStatus_FAILED :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatus_FAILED = StackSetDriftDetectionStatus' "FAILED"

pattern StackSetDriftDetectionStatus_IN_PROGRESS :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatus_IN_PROGRESS = StackSetDriftDetectionStatus' "IN_PROGRESS"

pattern StackSetDriftDetectionStatus_PARTIAL_SUCCESS :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatus_PARTIAL_SUCCESS = StackSetDriftDetectionStatus' "PARTIAL_SUCCESS"

pattern StackSetDriftDetectionStatus_STOPPED :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatus_STOPPED = StackSetDriftDetectionStatus' "STOPPED"

{-# COMPLETE
  StackSetDriftDetectionStatus_COMPLETED,
  StackSetDriftDetectionStatus_FAILED,
  StackSetDriftDetectionStatus_IN_PROGRESS,
  StackSetDriftDetectionStatus_PARTIAL_SUCCESS,
  StackSetDriftDetectionStatus_STOPPED,
  StackSetDriftDetectionStatus'
  #-}
