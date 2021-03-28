{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
  ( OrganizationDataSourceConfigurations (..)
  -- * Smart constructor
  , mkOrganizationDataSourceConfigurations
  -- * Lenses
  , odscS3Logs
  ) where

import qualified Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains information on which data sources will be configured to be automatically enabled for new members within the organization.
--
-- /See:/ 'mkOrganizationDataSourceConfigurations' smart constructor.
newtype OrganizationDataSourceConfigurations = OrganizationDataSourceConfigurations'
  { s3Logs :: Core.Maybe Types.OrganizationS3LogsConfiguration
    -- ^ Describes whether S3 data event logs are enabled for new members of the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationDataSourceConfigurations' value with any optional fields omitted.
mkOrganizationDataSourceConfigurations
    :: OrganizationDataSourceConfigurations
mkOrganizationDataSourceConfigurations
  = OrganizationDataSourceConfigurations'{s3Logs = Core.Nothing}

-- | Describes whether S3 data event logs are enabled for new members of the organization.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odscS3Logs :: Lens.Lens' OrganizationDataSourceConfigurations (Core.Maybe Types.OrganizationS3LogsConfiguration)
odscS3Logs = Lens.field @"s3Logs"
{-# INLINEABLE odscS3Logs #-}
{-# DEPRECATED s3Logs "Use generic-lens or generic-optics with 's3Logs' instead"  #-}

instance Core.FromJSON OrganizationDataSourceConfigurations where
        toJSON OrganizationDataSourceConfigurations{..}
          = Core.object (Core.catMaybes [("s3Logs" Core..=) Core.<$> s3Logs])
