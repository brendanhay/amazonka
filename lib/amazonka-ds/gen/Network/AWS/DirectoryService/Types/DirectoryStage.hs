{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryStage
  ( DirectoryStage
      ( DirectoryStage',
        DirectoryStageRequested,
        DirectoryStageCreating,
        DirectoryStageCreated,
        DirectoryStageActive,
        DirectoryStageInoperable,
        DirectoryStageImpaired,
        DirectoryStageRestoring,
        DirectoryStageRestoreFailed,
        DirectoryStageDeleting,
        DirectoryStageDeleted,
        DirectoryStageFailed,
        fromDirectoryStage
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DirectoryStage = DirectoryStage'
  { fromDirectoryStage ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DirectoryStageRequested :: DirectoryStage
pattern DirectoryStageRequested = DirectoryStage' "Requested"

pattern DirectoryStageCreating :: DirectoryStage
pattern DirectoryStageCreating = DirectoryStage' "Creating"

pattern DirectoryStageCreated :: DirectoryStage
pattern DirectoryStageCreated = DirectoryStage' "Created"

pattern DirectoryStageActive :: DirectoryStage
pattern DirectoryStageActive = DirectoryStage' "Active"

pattern DirectoryStageInoperable :: DirectoryStage
pattern DirectoryStageInoperable = DirectoryStage' "Inoperable"

pattern DirectoryStageImpaired :: DirectoryStage
pattern DirectoryStageImpaired = DirectoryStage' "Impaired"

pattern DirectoryStageRestoring :: DirectoryStage
pattern DirectoryStageRestoring = DirectoryStage' "Restoring"

pattern DirectoryStageRestoreFailed :: DirectoryStage
pattern DirectoryStageRestoreFailed = DirectoryStage' "RestoreFailed"

pattern DirectoryStageDeleting :: DirectoryStage
pattern DirectoryStageDeleting = DirectoryStage' "Deleting"

pattern DirectoryStageDeleted :: DirectoryStage
pattern DirectoryStageDeleted = DirectoryStage' "Deleted"

pattern DirectoryStageFailed :: DirectoryStage
pattern DirectoryStageFailed = DirectoryStage' "Failed"

{-# COMPLETE
  DirectoryStageRequested,
  DirectoryStageCreating,
  DirectoryStageCreated,
  DirectoryStageActive,
  DirectoryStageInoperable,
  DirectoryStageImpaired,
  DirectoryStageRestoring,
  DirectoryStageRestoreFailed,
  DirectoryStageDeleting,
  DirectoryStageDeleted,
  DirectoryStageFailed,
  DirectoryStage'
  #-}
