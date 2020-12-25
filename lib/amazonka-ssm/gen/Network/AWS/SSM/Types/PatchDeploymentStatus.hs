{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchDeploymentStatus
  ( PatchDeploymentStatus
      ( PatchDeploymentStatus',
        PatchDeploymentStatusApproved,
        PatchDeploymentStatusPendingApproval,
        PatchDeploymentStatusExplicitApproved,
        PatchDeploymentStatusExplicitRejected,
        fromPatchDeploymentStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PatchDeploymentStatus = PatchDeploymentStatus'
  { fromPatchDeploymentStatus ::
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

pattern PatchDeploymentStatusApproved :: PatchDeploymentStatus
pattern PatchDeploymentStatusApproved = PatchDeploymentStatus' "APPROVED"

pattern PatchDeploymentStatusPendingApproval :: PatchDeploymentStatus
pattern PatchDeploymentStatusPendingApproval = PatchDeploymentStatus' "PENDING_APPROVAL"

pattern PatchDeploymentStatusExplicitApproved :: PatchDeploymentStatus
pattern PatchDeploymentStatusExplicitApproved = PatchDeploymentStatus' "EXPLICIT_APPROVED"

pattern PatchDeploymentStatusExplicitRejected :: PatchDeploymentStatus
pattern PatchDeploymentStatusExplicitRejected = PatchDeploymentStatus' "EXPLICIT_REJECTED"

{-# COMPLETE
  PatchDeploymentStatusApproved,
  PatchDeploymentStatusPendingApproval,
  PatchDeploymentStatusExplicitApproved,
  PatchDeploymentStatusExplicitRejected,
  PatchDeploymentStatus'
  #-}
