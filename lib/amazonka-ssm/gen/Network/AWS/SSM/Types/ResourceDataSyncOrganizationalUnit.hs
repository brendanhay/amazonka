{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
  ( ResourceDataSyncOrganizationalUnit (..),

    -- * Smart constructor
    mkResourceDataSyncOrganizationalUnit,

    -- * Lenses
    rdsouOrganizationalUnitId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OrganizationalUnitId as Types

-- | The AWS Organizations organizational unit data source for the sync.
--
-- /See:/ 'mkResourceDataSyncOrganizationalUnit' smart constructor.
newtype ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { -- | The AWS Organization unit ID data source for the sync.
    organizationalUnitId :: Core.Maybe Types.OrganizationalUnitId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncOrganizationalUnit' value with any optional fields omitted.
mkResourceDataSyncOrganizationalUnit ::
  ResourceDataSyncOrganizationalUnit
mkResourceDataSyncOrganizationalUnit =
  ResourceDataSyncOrganizationalUnit'
    { organizationalUnitId =
        Core.Nothing
    }

-- | The AWS Organization unit ID data source for the sync.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsouOrganizationalUnitId :: Lens.Lens' ResourceDataSyncOrganizationalUnit (Core.Maybe Types.OrganizationalUnitId)
rdsouOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# DEPRECATED rdsouOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

instance Core.FromJSON ResourceDataSyncOrganizationalUnit where
  toJSON ResourceDataSyncOrganizationalUnit {..} =
    Core.object
      ( Core.catMaybes
          [("OrganizationalUnitId" Core..=) Core.<$> organizationalUnitId]
      )

instance Core.FromJSON ResourceDataSyncOrganizationalUnit where
  parseJSON =
    Core.withObject "ResourceDataSyncOrganizationalUnit" Core.$
      \x ->
        ResourceDataSyncOrganizationalUnit'
          Core.<$> (x Core..:? "OrganizationalUnitId")
