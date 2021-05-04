{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype StackSetDriftDetectionStatus = StackSetDriftDetectionStatus'
  { fromStackSetDriftDetectionStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
