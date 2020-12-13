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
        DSRequested,
        DSCreating,
        DSCreated,
        DSActive,
        DSInoperable,
        DSImpaired,
        DSRestoring,
        DSRestoreFailed,
        DSDeleting,
        DSDeleted,
        DSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DirectoryStage = DirectoryStage' Lude.Text
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

pattern DSRequested :: DirectoryStage
pattern DSRequested = DirectoryStage' "Requested"

pattern DSCreating :: DirectoryStage
pattern DSCreating = DirectoryStage' "Creating"

pattern DSCreated :: DirectoryStage
pattern DSCreated = DirectoryStage' "Created"

pattern DSActive :: DirectoryStage
pattern DSActive = DirectoryStage' "Active"

pattern DSInoperable :: DirectoryStage
pattern DSInoperable = DirectoryStage' "Inoperable"

pattern DSImpaired :: DirectoryStage
pattern DSImpaired = DirectoryStage' "Impaired"

pattern DSRestoring :: DirectoryStage
pattern DSRestoring = DirectoryStage' "Restoring"

pattern DSRestoreFailed :: DirectoryStage
pattern DSRestoreFailed = DirectoryStage' "RestoreFailed"

pattern DSDeleting :: DirectoryStage
pattern DSDeleting = DirectoryStage' "Deleting"

pattern DSDeleted :: DirectoryStage
pattern DSDeleted = DirectoryStage' "Deleted"

pattern DSFailed :: DirectoryStage
pattern DSFailed = DirectoryStage' "Failed"

{-# COMPLETE
  DSRequested,
  DSCreating,
  DSCreated,
  DSActive,
  DSInoperable,
  DSImpaired,
  DSRestoring,
  DSRestoreFailed,
  DSDeleting,
  DSDeleted,
  DSFailed,
  DirectoryStage'
  #-}
