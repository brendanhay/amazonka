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

import qualified Network.AWS.Prelude as Prelude

newtype NotebookInstanceStatus = NotebookInstanceStatus'
  { fromNotebookInstanceStatus ::
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
