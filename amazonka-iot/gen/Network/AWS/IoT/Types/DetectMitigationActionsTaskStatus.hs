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

import qualified Network.AWS.Prelude as Prelude

newtype DetectMitigationActionsTaskStatus = DetectMitigationActionsTaskStatus'
  { fromDetectMitigationActionsTaskStatus ::
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
