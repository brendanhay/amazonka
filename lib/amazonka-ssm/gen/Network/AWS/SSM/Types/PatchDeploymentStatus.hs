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
        Approved,
        ExplicitApproved,
        ExplicitRejected,
        PendingApproval
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PatchDeploymentStatus = PatchDeploymentStatus' Lude.Text
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

pattern Approved :: PatchDeploymentStatus
pattern Approved = PatchDeploymentStatus' "APPROVED"

pattern ExplicitApproved :: PatchDeploymentStatus
pattern ExplicitApproved = PatchDeploymentStatus' "EXPLICIT_APPROVED"

pattern ExplicitRejected :: PatchDeploymentStatus
pattern ExplicitRejected = PatchDeploymentStatus' "EXPLICIT_REJECTED"

pattern PendingApproval :: PatchDeploymentStatus
pattern PendingApproval = PatchDeploymentStatus' "PENDING_APPROVAL"

{-# COMPLETE
  Approved,
  ExplicitApproved,
  ExplicitRejected,
  PendingApproval,
  PatchDeploymentStatus'
  #-}
