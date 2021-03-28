{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
  ( ServiceManagedDatastoreS3StorageSummary (..)
  -- * Smart constructor
  , mkServiceManagedDatastoreS3StorageSummary
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
-- /See:/ 'mkServiceManagedDatastoreS3StorageSummary' smart constructor.
data ServiceManagedDatastoreS3StorageSummary = ServiceManagedDatastoreS3StorageSummary'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceManagedDatastoreS3StorageSummary' value with any optional fields omitted.
mkServiceManagedDatastoreS3StorageSummary
    :: ServiceManagedDatastoreS3StorageSummary
mkServiceManagedDatastoreS3StorageSummary
  = ServiceManagedDatastoreS3StorageSummary'

instance Core.FromJSON ServiceManagedDatastoreS3StorageSummary
         where
        parseJSON
          = Core.withObject "ServiceManagedDatastoreS3StorageSummary" Core.$
              \ x -> Core.pure ServiceManagedDatastoreS3StorageSummary'
