{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccelerateConfiguration
  ( AccelerateConfiguration (..),

    -- * Smart constructor
    mkAccelerateConfiguration,

    -- * Lenses
    acStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketAccelerateStatus

-- | Configures the transfer acceleration state for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Amazon S3 Transfer Acceleration> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkAccelerateConfiguration' smart constructor.
newtype AccelerateConfiguration = AccelerateConfiguration'
  { -- | Specifies the transfer acceleration status of the bucket.
    status :: Lude.Maybe BucketAccelerateStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccelerateConfiguration' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the transfer acceleration status of the bucket.
mkAccelerateConfiguration ::
  AccelerateConfiguration
mkAccelerateConfiguration =
  AccelerateConfiguration' {status = Lude.Nothing}

-- | Specifies the transfer acceleration status of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acStatus :: Lens.Lens' AccelerateConfiguration (Lude.Maybe BucketAccelerateStatus)
acStatus = Lens.lens (status :: AccelerateConfiguration -> Lude.Maybe BucketAccelerateStatus) (\s a -> s {status = a} :: AccelerateConfiguration)
{-# DEPRECATED acStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToXML AccelerateConfiguration where
  toXML AccelerateConfiguration' {..} =
    Lude.mconcat ["Status" Lude.@= status]
