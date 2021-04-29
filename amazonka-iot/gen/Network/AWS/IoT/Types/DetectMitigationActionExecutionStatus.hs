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
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionExecutionStatus
  ( DetectMitigationActionExecutionStatus
      ( ..,
        DetectMitigationActionExecutionStatus_FAILED,
        DetectMitigationActionExecutionStatus_IN_PROGRESS,
        DetectMitigationActionExecutionStatus_SKIPPED,
        DetectMitigationActionExecutionStatus_SUCCESSFUL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DetectMitigationActionExecutionStatus = DetectMitigationActionExecutionStatus'
  { fromDetectMitigationActionExecutionStatus ::
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

pattern DetectMitigationActionExecutionStatus_FAILED :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_FAILED = DetectMitigationActionExecutionStatus' "FAILED"

pattern DetectMitigationActionExecutionStatus_IN_PROGRESS :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_IN_PROGRESS = DetectMitigationActionExecutionStatus' "IN_PROGRESS"

pattern DetectMitigationActionExecutionStatus_SKIPPED :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_SKIPPED = DetectMitigationActionExecutionStatus' "SKIPPED"

pattern DetectMitigationActionExecutionStatus_SUCCESSFUL :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_SUCCESSFUL = DetectMitigationActionExecutionStatus' "SUCCESSFUL"

{-# COMPLETE
  DetectMitigationActionExecutionStatus_FAILED,
  DetectMitigationActionExecutionStatus_IN_PROGRESS,
  DetectMitigationActionExecutionStatus_SKIPPED,
  DetectMitigationActionExecutionStatus_SUCCESSFUL,
  DetectMitigationActionExecutionStatus'
  #-}
