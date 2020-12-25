{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
  ( ImmediateModeScheduleActionStartSettings (..),

    -- * Smart constructor
    mkImmediateModeScheduleActionStartSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'mkImmediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImmediateModeScheduleActionStartSettings' value with any optional fields omitted.
mkImmediateModeScheduleActionStartSettings ::
  ImmediateModeScheduleActionStartSettings
mkImmediateModeScheduleActionStartSettings =
  ImmediateModeScheduleActionStartSettings'

instance Core.FromJSON ImmediateModeScheduleActionStartSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON ImmediateModeScheduleActionStartSettings where
  parseJSON =
    Core.withObject "ImmediateModeScheduleActionStartSettings" Core.$
      \x -> Core.pure ImmediateModeScheduleActionStartSettings'
