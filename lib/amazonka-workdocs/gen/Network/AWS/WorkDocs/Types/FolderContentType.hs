{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderContentType
  ( FolderContentType
      ( FolderContentType',
        All,
        Document,
        Folder
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FolderContentType = FolderContentType' Lude.Text
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

pattern All :: FolderContentType
pattern All = FolderContentType' "ALL"

pattern Document :: FolderContentType
pattern Document = FolderContentType' "DOCUMENT"

pattern Folder :: FolderContentType
pattern Folder = FolderContentType' "FOLDER"

{-# COMPLETE
  All,
  Document,
  Folder,
  FolderContentType'
  #-}
