{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.FolderName
  ( FolderName
    ( FolderName'
    , FolderNameInbox
    , FolderNameDeletedItems
    , FolderNameSentItems
    , FolderNameDrafts
    , FolderNameJunkEmail
    , fromFolderName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype FolderName = FolderName'{fromFolderName :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern FolderNameInbox :: FolderName
pattern FolderNameInbox = FolderName' "INBOX"

pattern FolderNameDeletedItems :: FolderName
pattern FolderNameDeletedItems = FolderName' "DELETED_ITEMS"

pattern FolderNameSentItems :: FolderName
pattern FolderNameSentItems = FolderName' "SENT_ITEMS"

pattern FolderNameDrafts :: FolderName
pattern FolderNameDrafts = FolderName' "DRAFTS"

pattern FolderNameJunkEmail :: FolderName
pattern FolderNameJunkEmail = FolderName' "JUNK_EMAIL"

{-# COMPLETE 
  FolderNameInbox,

  FolderNameDeletedItems,

  FolderNameSentItems,

  FolderNameDrafts,

  FolderNameJunkEmail,
  FolderName'
  #-}
