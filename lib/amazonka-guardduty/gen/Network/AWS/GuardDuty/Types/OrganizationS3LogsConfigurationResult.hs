{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
  ( OrganizationS3LogsConfigurationResult (..),

    -- * Smart constructor
    mkOrganizationS3LogsConfigurationResult,

    -- * Lenses
    oslcrAutoEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current configuration of S3 data event logs as a data source for the organization.
--
-- /See:/ 'mkOrganizationS3LogsConfigurationResult' smart constructor.
newtype OrganizationS3LogsConfigurationResult = OrganizationS3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
    autoEnable :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationS3LogsConfigurationResult' value with any optional fields omitted.
mkOrganizationS3LogsConfigurationResult ::
  -- | 'autoEnable'
  Core.Bool ->
  OrganizationS3LogsConfigurationResult
mkOrganizationS3LogsConfigurationResult autoEnable =
  OrganizationS3LogsConfigurationResult' {autoEnable}

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oslcrAutoEnable :: Lens.Lens' OrganizationS3LogsConfigurationResult Core.Bool
oslcrAutoEnable = Lens.field @"autoEnable"
{-# DEPRECATED oslcrAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

instance Core.FromJSON OrganizationS3LogsConfigurationResult where
  parseJSON =
    Core.withObject "OrganizationS3LogsConfigurationResult" Core.$
      \x ->
        OrganizationS3LogsConfigurationResult'
          Core.<$> (x Core..: "autoEnable")
