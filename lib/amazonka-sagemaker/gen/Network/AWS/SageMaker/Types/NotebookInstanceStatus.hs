-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceStatus
  ( NotebookInstanceStatus
      ( NotebookInstanceStatus',
        NISDeleting,
        NISFailed,
        NISInService,
        NISPending,
        NISStopped,
        NISStopping,
        NISUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotebookInstanceStatus = NotebookInstanceStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern NISDeleting :: NotebookInstanceStatus
pattern NISDeleting = NotebookInstanceStatus' "Deleting"

pattern NISFailed :: NotebookInstanceStatus
pattern NISFailed = NotebookInstanceStatus' "Failed"

pattern NISInService :: NotebookInstanceStatus
pattern NISInService = NotebookInstanceStatus' "InService"

pattern NISPending :: NotebookInstanceStatus
pattern NISPending = NotebookInstanceStatus' "Pending"

pattern NISStopped :: NotebookInstanceStatus
pattern NISStopped = NotebookInstanceStatus' "Stopped"

pattern NISStopping :: NotebookInstanceStatus
pattern NISStopping = NotebookInstanceStatus' "Stopping"

pattern NISUpdating :: NotebookInstanceStatus
pattern NISUpdating = NotebookInstanceStatus' "Updating"

{-# COMPLETE
  NISDeleting,
  NISFailed,
  NISInService,
  NISPending,
  NISStopped,
  NISStopping,
  NISUpdating,
  NotebookInstanceStatus'
  #-}
