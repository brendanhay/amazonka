{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
  ( NotebookInstanceSortOrder
      ( NotebookInstanceSortOrder',
        NISOAscending,
        NISODescending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotebookInstanceSortOrder = NotebookInstanceSortOrder' Lude.Text
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

pattern NISOAscending :: NotebookInstanceSortOrder
pattern NISOAscending = NotebookInstanceSortOrder' "Ascending"

pattern NISODescending :: NotebookInstanceSortOrder
pattern NISODescending = NotebookInstanceSortOrder' "Descending"

{-# COMPLETE
  NISOAscending,
  NISODescending,
  NotebookInstanceSortOrder'
  #-}
