{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
  ( OrganizationS3LogsConfiguration (..)
  -- * Smart constructor
  , mkOrganizationS3LogsConfiguration
  -- * Lenses
  , oslcAutoEnable
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether S3 data event logs will be automatically enabled for new members of the organization.
--
-- /See:/ 'mkOrganizationS3LogsConfiguration' smart constructor.
newtype OrganizationS3LogsConfiguration = OrganizationS3LogsConfiguration'
  { autoEnable :: Core.Bool
    -- ^ A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationS3LogsConfiguration' value with any optional fields omitted.
mkOrganizationS3LogsConfiguration
    :: Core.Bool -- ^ 'autoEnable'
    -> OrganizationS3LogsConfiguration
mkOrganizationS3LogsConfiguration autoEnable
  = OrganizationS3LogsConfiguration'{autoEnable}

-- | A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oslcAutoEnable :: Lens.Lens' OrganizationS3LogsConfiguration Core.Bool
oslcAutoEnable = Lens.field @"autoEnable"
{-# INLINEABLE oslcAutoEnable #-}
{-# DEPRECATED autoEnable "Use generic-lens or generic-optics with 'autoEnable' instead"  #-}

instance Core.FromJSON OrganizationS3LogsConfiguration where
        toJSON OrganizationS3LogsConfiguration{..}
          = Core.object
              (Core.catMaybes [Core.Just ("autoEnable" Core..= autoEnable)])
