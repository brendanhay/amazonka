{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns an assigned Amazon EBS volume. The volume remains registered with the stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UnassignVolume
  ( -- * Creating a request
    UnassignVolume (..),
    mkUnassignVolume,

    -- ** Request lenses
    uVolumeId,

    -- * Destructuring the response
    UnassignVolumeResponse (..),
    mkUnassignVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnassignVolume' smart constructor.
newtype UnassignVolume = UnassignVolume'
  { -- | The volume ID.
    volumeId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignVolume' with the minimum fields required to make a request.
--
-- * 'volumeId' - The volume ID.
mkUnassignVolume ::
  -- | 'volumeId'
  Lude.Text ->
  UnassignVolume
mkUnassignVolume pVolumeId_ =
  UnassignVolume' {volumeId = pVolumeId_}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uVolumeId :: Lens.Lens' UnassignVolume Lude.Text
uVolumeId = Lens.lens (volumeId :: UnassignVolume -> Lude.Text) (\s a -> s {volumeId = a} :: UnassignVolume)
{-# DEPRECATED uVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest UnassignVolume where
  type Rs UnassignVolume = UnassignVolumeResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UnassignVolumeResponse'

instance Lude.ToHeaders UnassignVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UnassignVolume" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnassignVolume where
  toJSON UnassignVolume' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeId" Lude..= volumeId)])

instance Lude.ToPath UnassignVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery UnassignVolume where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnassignVolumeResponse' smart constructor.
data UnassignVolumeResponse = UnassignVolumeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignVolumeResponse' with the minimum fields required to make a request.
mkUnassignVolumeResponse ::
  UnassignVolumeResponse
mkUnassignVolumeResponse = UnassignVolumeResponse'
