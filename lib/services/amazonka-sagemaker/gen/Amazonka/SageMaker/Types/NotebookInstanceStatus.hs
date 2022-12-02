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
-- Module      : Amazonka.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NotebookInstanceStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotebookInstanceStatus = NotebookInstanceStatus'
  { fromNotebookInstanceStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
