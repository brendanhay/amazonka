{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
  ( S3LogsConfigurationResult (..),

    -- * Smart constructor
    mkS3LogsConfigurationResult,

    -- * Lenses
    slcrStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types.DataSourceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'mkS3LogsConfigurationResult' smart constructor.
newtype S3LogsConfigurationResult = S3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
    status :: Types.DataSourceStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'S3LogsConfigurationResult' value with any optional fields omitted.
mkS3LogsConfigurationResult ::
  -- | 'status'
  Types.DataSourceStatus ->
  S3LogsConfigurationResult
mkS3LogsConfigurationResult status =
  S3LogsConfigurationResult' {status}

-- | A value that describes whether S3 data event logs are automatically enabled for new members of the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcrStatus :: Lens.Lens' S3LogsConfigurationResult Types.DataSourceStatus
slcrStatus = Lens.field @"status"
{-# DEPRECATED slcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON S3LogsConfigurationResult where
  parseJSON =
    Core.withObject "S3LogsConfigurationResult" Core.$
      \x -> S3LogsConfigurationResult' Core.<$> (x Core..: "status")
