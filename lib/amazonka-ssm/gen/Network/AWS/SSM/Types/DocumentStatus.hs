{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentStatus
  ( DocumentStatus
      ( DocumentStatus',
        DSCreating,
        DSActive,
        DSUpdating,
        DSDeleting,
        DSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The status of a document.
newtype DocumentStatus = DocumentStatus' Lude.Text
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

pattern DSCreating :: DocumentStatus
pattern DSCreating = DocumentStatus' "Creating"

pattern DSActive :: DocumentStatus
pattern DSActive = DocumentStatus' "Active"

pattern DSUpdating :: DocumentStatus
pattern DSUpdating = DocumentStatus' "Updating"

pattern DSDeleting :: DocumentStatus
pattern DSDeleting = DocumentStatus' "Deleting"

pattern DSFailed :: DocumentStatus
pattern DSFailed = DocumentStatus' "Failed"

{-# COMPLETE
  DSCreating,
  DSActive,
  DSUpdating,
  DSDeleting,
  DSFailed,
  DocumentStatus'
  #-}
