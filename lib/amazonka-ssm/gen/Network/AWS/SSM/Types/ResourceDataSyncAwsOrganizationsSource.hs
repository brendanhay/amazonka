{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncAwsOrganizationsSource
  ( ResourceDataSyncAwsOrganizationsSource (..)
  -- * Smart constructor
  , mkResourceDataSyncAwsOrganizationsSource
  -- * Lenses
  , rdsaosOrganizationSourceType
  , rdsaosOrganizationalUnits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OrganizationSourceType as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit as Types

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations or, if an AWS Organization is not present, from multiple AWS Regions.
--
-- /See:/ 'mkResourceDataSyncAwsOrganizationsSource' smart constructor.
data ResourceDataSyncAwsOrganizationsSource = ResourceDataSyncAwsOrganizationsSource'
  { organizationSourceType :: Types.OrganizationSourceType
    -- ^ If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization. 
  , organizationalUnits :: Core.Maybe (Core.NonEmpty Types.ResourceDataSyncOrganizationalUnit)
    -- ^ The AWS Organizations organization units included in the sync.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncAwsOrganizationsSource' value with any optional fields omitted.
mkResourceDataSyncAwsOrganizationsSource
    :: Types.OrganizationSourceType -- ^ 'organizationSourceType'
    -> ResourceDataSyncAwsOrganizationsSource
mkResourceDataSyncAwsOrganizationsSource organizationSourceType
  = ResourceDataSyncAwsOrganizationsSource'{organizationSourceType,
                                            organizationalUnits = Core.Nothing}

-- | If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization. 
--
-- /Note:/ Consider using 'organizationSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsaosOrganizationSourceType :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource Types.OrganizationSourceType
rdsaosOrganizationSourceType = Lens.field @"organizationSourceType"
{-# INLINEABLE rdsaosOrganizationSourceType #-}
{-# DEPRECATED organizationSourceType "Use generic-lens or generic-optics with 'organizationSourceType' instead"  #-}

-- | The AWS Organizations organization units included in the sync.
--
-- /Note:/ Consider using 'organizationalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsaosOrganizationalUnits :: Lens.Lens' ResourceDataSyncAwsOrganizationsSource (Core.Maybe (Core.NonEmpty Types.ResourceDataSyncOrganizationalUnit))
rdsaosOrganizationalUnits = Lens.field @"organizationalUnits"
{-# INLINEABLE rdsaosOrganizationalUnits #-}
{-# DEPRECATED organizationalUnits "Use generic-lens or generic-optics with 'organizationalUnits' instead"  #-}

instance Core.FromJSON ResourceDataSyncAwsOrganizationsSource where
        toJSON ResourceDataSyncAwsOrganizationsSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("OrganizationSourceType" Core..= organizationSourceType),
                  ("OrganizationalUnits" Core..=) Core.<$> organizationalUnits])

instance Core.FromJSON ResourceDataSyncAwsOrganizationsSource where
        parseJSON
          = Core.withObject "ResourceDataSyncAwsOrganizationsSource" Core.$
              \ x ->
                ResourceDataSyncAwsOrganizationsSource' Core.<$>
                  (x Core..: "OrganizationSourceType") Core.<*>
                    x Core..:? "OrganizationalUnits"
