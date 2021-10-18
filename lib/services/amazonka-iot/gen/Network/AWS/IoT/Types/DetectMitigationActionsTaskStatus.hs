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
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionsTaskStatus
  ( DetectMitigationActionsTaskStatus
      ( ..,
        DetectMitigationActionsTaskStatus_CANCELED,
        DetectMitigationActionsTaskStatus_FAILED,
        DetectMitigationActionsTaskStatus_IN_PROGRESS,
        DetectMitigationActionsTaskStatus_SUCCESSFUL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DetectMitigationActionsTaskStatus = DetectMitigationActionsTaskStatus'
  { fromDetectMitigationActionsTaskStatus ::
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

pattern DetectMitigationActionsTaskStatus_CANCELED :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_CANCELED = DetectMitigationActionsTaskStatus' "CANCELED"

pattern DetectMitigationActionsTaskStatus_FAILED :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_FAILED = DetectMitigationActionsTaskStatus' "FAILED"

pattern DetectMitigationActionsTaskStatus_IN_PROGRESS :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_IN_PROGRESS = DetectMitigationActionsTaskStatus' "IN_PROGRESS"

pattern DetectMitigationActionsTaskStatus_SUCCESSFUL :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_SUCCESSFUL = DetectMitigationActionsTaskStatus' "SUCCESSFUL"

{-# COMPLETE
  DetectMitigationActionsTaskStatus_CANCELED,
  DetectMitigationActionsTaskStatus_FAILED,
  DetectMitigationActionsTaskStatus_IN_PROGRESS,
  DetectMitigationActionsTaskStatus_SUCCESSFUL,
  DetectMitigationActionsTaskStatus'
  #-}
