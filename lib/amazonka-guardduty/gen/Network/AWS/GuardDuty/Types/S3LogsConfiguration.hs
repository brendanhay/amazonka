{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfiguration
  ( S3LogsConfiguration (..),

    -- * Smart constructor
    mkS3LogsConfiguration,

    -- * Lenses
    slcEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'mkS3LogsConfiguration' smart constructor.
newtype S3LogsConfiguration = S3LogsConfiguration'
  { -- | The status of S3 data event logs as a data source.
    enable :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'S3LogsConfiguration' value with any optional fields omitted.
mkS3LogsConfiguration ::
  -- | 'enable'
  Core.Bool ->
  S3LogsConfiguration
mkS3LogsConfiguration enable = S3LogsConfiguration' {enable}

-- | The status of S3 data event logs as a data source.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEnable :: Lens.Lens' S3LogsConfiguration Core.Bool
slcEnable = Lens.field @"enable"
{-# DEPRECATED slcEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

instance Core.FromJSON S3LogsConfiguration where
  toJSON S3LogsConfiguration {..} =
    Core.object
      (Core.catMaybes [Core.Just ("enable" Core..= enable)])
