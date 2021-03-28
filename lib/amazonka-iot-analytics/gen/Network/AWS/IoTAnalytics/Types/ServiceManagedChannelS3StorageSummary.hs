{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
  ( ServiceManagedChannelS3StorageSummary (..)
  -- * Smart constructor
  , mkServiceManagedChannelS3StorageSummary
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- /See:/ 'mkServiceManagedChannelS3StorageSummary' smart constructor.
data ServiceManagedChannelS3StorageSummary = ServiceManagedChannelS3StorageSummary'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceManagedChannelS3StorageSummary' value with any optional fields omitted.
mkServiceManagedChannelS3StorageSummary
    :: ServiceManagedChannelS3StorageSummary
mkServiceManagedChannelS3StorageSummary
  = ServiceManagedChannelS3StorageSummary'

instance Core.FromJSON ServiceManagedChannelS3StorageSummary where
        parseJSON
          = Core.withObject "ServiceManagedChannelS3StorageSummary" Core.$
              \ x -> Core.pure ServiceManagedChannelS3StorageSummary'
