{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
  ( OrganizationDataSourceConfigurationsResult (..)
  -- * Smart constructor
  , mkOrganizationDataSourceConfigurationsResult
  -- * Lenses
  , odscrS3Logs
  ) where

import qualified Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains information on which data sources are automatically enabled for new members within the organization.
--
-- /See:/ 'mkOrganizationDataSourceConfigurationsResult' smart constructor.
newtype OrganizationDataSourceConfigurationsResult = OrganizationDataSourceConfigurationsResult'
  { s3Logs :: Types.OrganizationS3LogsConfigurationResult
    -- ^ Describes whether S3 data event logs are enabled as a data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationDataSourceConfigurationsResult' value with any optional fields omitted.
mkOrganizationDataSourceConfigurationsResult
    :: Types.OrganizationS3LogsConfigurationResult -- ^ 's3Logs'
    -> OrganizationDataSourceConfigurationsResult
mkOrganizationDataSourceConfigurationsResult s3Logs
  = OrganizationDataSourceConfigurationsResult'{s3Logs}

-- | Describes whether S3 data event logs are enabled as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odscrS3Logs :: Lens.Lens' OrganizationDataSourceConfigurationsResult Types.OrganizationS3LogsConfigurationResult
odscrS3Logs = Lens.field @"s3Logs"
{-# INLINEABLE odscrS3Logs #-}
{-# DEPRECATED s3Logs "Use generic-lens or generic-optics with 's3Logs' instead"  #-}

instance Core.FromJSON OrganizationDataSourceConfigurationsResult
         where
        parseJSON
          = Core.withObject "OrganizationDataSourceConfigurationsResult"
              Core.$
              \ x ->
                OrganizationDataSourceConfigurationsResult' Core.<$>
                  (x Core..: "s3Logs")
