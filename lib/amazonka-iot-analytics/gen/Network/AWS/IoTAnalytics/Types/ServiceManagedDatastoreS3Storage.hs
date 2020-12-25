{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
  ( ServiceManagedDatastoreS3Storage (..),

    -- * Smart constructor
    mkServiceManagedDatastoreS3Storage,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /See:/ 'mkServiceManagedDatastoreS3Storage' smart constructor.
data ServiceManagedDatastoreS3Storage = ServiceManagedDatastoreS3Storage'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceManagedDatastoreS3Storage' value with any optional fields omitted.
mkServiceManagedDatastoreS3Storage ::
  ServiceManagedDatastoreS3Storage
mkServiceManagedDatastoreS3Storage =
  ServiceManagedDatastoreS3Storage'

instance Core.FromJSON ServiceManagedDatastoreS3Storage where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON ServiceManagedDatastoreS3Storage where
  parseJSON =
    Core.withObject "ServiceManagedDatastoreS3Storage" Core.$
      \x -> Core.pure ServiceManagedDatastoreS3Storage'
