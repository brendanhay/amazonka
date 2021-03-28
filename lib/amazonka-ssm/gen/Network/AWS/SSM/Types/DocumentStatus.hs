{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentStatus
  ( DocumentStatus
    ( DocumentStatus'
    , DocumentStatusCreating
    , DocumentStatusActive
    , DocumentStatusUpdating
    , DocumentStatusDeleting
    , DocumentStatusFailed
    , fromDocumentStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The status of a document.
newtype DocumentStatus = DocumentStatus'{fromDocumentStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern DocumentStatusCreating :: DocumentStatus
pattern DocumentStatusCreating = DocumentStatus' "Creating"

pattern DocumentStatusActive :: DocumentStatus
pattern DocumentStatusActive = DocumentStatus' "Active"

pattern DocumentStatusUpdating :: DocumentStatus
pattern DocumentStatusUpdating = DocumentStatus' "Updating"

pattern DocumentStatusDeleting :: DocumentStatus
pattern DocumentStatusDeleting = DocumentStatus' "Deleting"

pattern DocumentStatusFailed :: DocumentStatus
pattern DocumentStatusFailed = DocumentStatus' "Failed"

{-# COMPLETE 
  DocumentStatusCreating,

  DocumentStatusActive,

  DocumentStatusUpdating,

  DocumentStatusDeleting,

  DocumentStatusFailed,
  DocumentStatus'
  #-}
