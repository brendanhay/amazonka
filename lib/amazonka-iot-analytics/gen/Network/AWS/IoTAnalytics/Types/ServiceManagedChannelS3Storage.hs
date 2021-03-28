{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
  ( ServiceManagedChannelS3Storage (..)
  -- * Smart constructor
  , mkServiceManagedChannelS3Storage
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /See:/ 'mkServiceManagedChannelS3Storage' smart constructor.
data ServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceManagedChannelS3Storage' value with any optional fields omitted.
mkServiceManagedChannelS3Storage
    :: ServiceManagedChannelS3Storage
mkServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'

instance Core.FromJSON ServiceManagedChannelS3Storage where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON ServiceManagedChannelS3Storage where
        parseJSON
          = Core.withObject "ServiceManagedChannelS3Storage" Core.$
              \ x -> Core.pure ServiceManagedChannelS3Storage'
