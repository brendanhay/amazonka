{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I/O operations for a volume that had I/O operations disabled because the data on the volume was potentially inconsistent.
module Network.AWS.EC2.EnableVolumeIO
  ( -- * Creating a request
    EnableVolumeIO (..),
    mkEnableVolumeIO,

    -- ** Request lenses
    evioDryRun,
    evioVolumeId,

    -- * Destructuring the response
    EnableVolumeIOResponse (..),
    mkEnableVolumeIOResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableVolumeIO' smart constructor.
data EnableVolumeIO = EnableVolumeIO'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    volumeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableVolumeIO' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'volumeId' - The ID of the volume.
mkEnableVolumeIO ::
  -- | 'volumeId'
  Lude.Text ->
  EnableVolumeIO
mkEnableVolumeIO pVolumeId_ =
  EnableVolumeIO' {dryRun = Lude.Nothing, volumeId = pVolumeId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioDryRun :: Lens.Lens' EnableVolumeIO (Lude.Maybe Lude.Bool)
evioDryRun = Lens.lens (dryRun :: EnableVolumeIO -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableVolumeIO)
{-# DEPRECATED evioDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioVolumeId :: Lens.Lens' EnableVolumeIO Lude.Text
evioVolumeId = Lens.lens (volumeId :: EnableVolumeIO -> Lude.Text) (\s a -> s {volumeId = a} :: EnableVolumeIO)
{-# DEPRECATED evioVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest EnableVolumeIO where
  type Rs EnableVolumeIO = EnableVolumeIOResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull EnableVolumeIOResponse'

instance Lude.ToHeaders EnableVolumeIO where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableVolumeIO where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableVolumeIO where
  toQuery EnableVolumeIO' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableVolumeIO" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VolumeId" Lude.=: volumeId
      ]

-- | /See:/ 'mkEnableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableVolumeIOResponse' with the minimum fields required to make a request.
mkEnableVolumeIOResponse ::
  EnableVolumeIOResponse
mkEnableVolumeIOResponse = EnableVolumeIOResponse'
