{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSortKey
  ( NotebookInstanceSortKey
      ( NotebookInstanceSortKey',
        NISKCreationTime,
        NISKName,
        NISKStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotebookInstanceSortKey = NotebookInstanceSortKey' Lude.Text
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

pattern NISKCreationTime :: NotebookInstanceSortKey
pattern NISKCreationTime = NotebookInstanceSortKey' "CreationTime"

pattern NISKName :: NotebookInstanceSortKey
pattern NISKName = NotebookInstanceSortKey' "Name"

pattern NISKStatus :: NotebookInstanceSortKey
pattern NISKStatus = NotebookInstanceSortKey' "Status"

{-# COMPLETE
  NISKCreationTime,
  NISKName,
  NISKStatus,
  NotebookInstanceSortKey'
  #-}
