-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusStatus
  ( RadiusStatus
      ( RadiusStatus',
        RSCompleted,
        RSCreating,
        RSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RadiusStatus = RadiusStatus' Lude.Text
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

pattern RSCompleted :: RadiusStatus
pattern RSCompleted = RadiusStatus' "Completed"

pattern RSCreating :: RadiusStatus
pattern RSCreating = RadiusStatus' "Creating"

pattern RSFailed :: RadiusStatus
pattern RSFailed = RadiusStatus' "Failed"

{-# COMPLETE
  RSCompleted,
  RSCreating,
  RSFailed,
  RadiusStatus'
  #-}
