{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageStatus
  ( ImageStatus
      ( ImageStatus',
        ImageStatusCreating,
        ImageStatusCreated,
        ImageStatusCreateFailed,
        ImageStatusUpdating,
        ImageStatusUpdateFailed,
        ImageStatusDeleting,
        ImageStatusDeleteFailed,
        fromImageStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImageStatus = ImageStatus' {fromImageStatus :: Core.Text}
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

pattern ImageStatusCreating :: ImageStatus
pattern ImageStatusCreating = ImageStatus' "CREATING"

pattern ImageStatusCreated :: ImageStatus
pattern ImageStatusCreated = ImageStatus' "CREATED"

pattern ImageStatusCreateFailed :: ImageStatus
pattern ImageStatusCreateFailed = ImageStatus' "CREATE_FAILED"

pattern ImageStatusUpdating :: ImageStatus
pattern ImageStatusUpdating = ImageStatus' "UPDATING"

pattern ImageStatusUpdateFailed :: ImageStatus
pattern ImageStatusUpdateFailed = ImageStatus' "UPDATE_FAILED"

pattern ImageStatusDeleting :: ImageStatus
pattern ImageStatusDeleting = ImageStatus' "DELETING"

pattern ImageStatusDeleteFailed :: ImageStatus
pattern ImageStatusDeleteFailed = ImageStatus' "DELETE_FAILED"

{-# COMPLETE
  ImageStatusCreating,
  ImageStatusCreated,
  ImageStatusCreateFailed,
  ImageStatusUpdating,
  ImageStatusUpdateFailed,
  ImageStatusDeleting,
  ImageStatusDeleteFailed,
  ImageStatus'
  #-}
