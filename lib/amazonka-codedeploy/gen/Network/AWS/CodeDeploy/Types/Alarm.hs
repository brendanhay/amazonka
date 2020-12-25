{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.Alarm
  ( Alarm (..),

    -- * Smart constructor
    mkAlarm,

    -- * Lenses
    aName,
  )
where

import qualified Network.AWS.CodeDeploy.Types.AlarmName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an alarm.
--
-- /See:/ 'mkAlarm' smart constructor.
newtype Alarm = Alarm'
  { -- | The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
    name :: Core.Maybe Types.AlarmName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Alarm' value with any optional fields omitted.
mkAlarm ::
  Alarm
mkAlarm = Alarm' {name = Core.Nothing}

-- | The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alarm (Core.Maybe Types.AlarmName)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON Alarm where
  toJSON Alarm {..} =
    Core.object (Core.catMaybes [("name" Core..=) Core.<$> name])

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject "Alarm" Core.$
      \x -> Alarm' Core.<$> (x Core..:? "name")
