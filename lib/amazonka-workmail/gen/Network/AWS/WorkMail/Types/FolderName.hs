-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.FolderName
  ( FolderName
      ( FolderName',
        DeletedItems,
        Drafts,
        Inbox,
        JunkEmail,
        SentItems
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FolderName = FolderName' Lude.Text
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

pattern DeletedItems :: FolderName
pattern DeletedItems = FolderName' "DELETED_ITEMS"

pattern Drafts :: FolderName
pattern Drafts = FolderName' "DRAFTS"

pattern Inbox :: FolderName
pattern Inbox = FolderName' "INBOX"

pattern JunkEmail :: FolderName
pattern JunkEmail = FolderName' "JUNK_EMAIL"

pattern SentItems :: FolderName
pattern SentItems = FolderName' "SENT_ITEMS"

{-# COMPLETE
  DeletedItems,
  Drafts,
  Inbox,
  JunkEmail,
  SentItems,
  FolderName'
  #-}
