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
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceStatus
  ( NotebookInstanceStatus
      ( ..,
        NotebookInstanceStatus_Deleting,
        NotebookInstanceStatus_Failed,
        NotebookInstanceStatus_InService,
        NotebookInstanceStatus_Pending,
        NotebookInstanceStatus_Stopped,
        NotebookInstanceStatus_Stopping,
        NotebookInstanceStatus_Updating
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype NotebookInstanceStatus = NotebookInstanceStatus'
  { fromNotebookInstanceStatus ::
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

pattern NotebookInstanceStatus_Deleting :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Deleting = NotebookInstanceStatus' "Deleting"

pattern NotebookInstanceStatus_Failed :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Failed = NotebookInstanceStatus' "Failed"

pattern NotebookInstanceStatus_InService :: NotebookInstanceStatus
pattern NotebookInstanceStatus_InService = NotebookInstanceStatus' "InService"

pattern NotebookInstanceStatus_Pending :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Pending = NotebookInstanceStatus' "Pending"

pattern NotebookInstanceStatus_Stopped :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Stopped = NotebookInstanceStatus' "Stopped"

pattern NotebookInstanceStatus_Stopping :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Stopping = NotebookInstanceStatus' "Stopping"

pattern NotebookInstanceStatus_Updating :: NotebookInstanceStatus
pattern NotebookInstanceStatus_Updating = NotebookInstanceStatus' "Updating"

{-# COMPLETE
  NotebookInstanceStatus_Deleting,
  NotebookInstanceStatus_Failed,
  NotebookInstanceStatus_InService,
  NotebookInstanceStatus_Pending,
  NotebookInstanceStatus_Stopped,
  NotebookInstanceStatus_Stopping,
  NotebookInstanceStatus_Updating,
  NotebookInstanceStatus'
  #-}
