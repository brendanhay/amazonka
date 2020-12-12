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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an alarm.
--
-- /See:/ 'mkAlarm' smart constructor.
newtype Alarm = Alarm' {name :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- * 'name' - The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
mkAlarm ::
  Alarm
mkAlarm = Alarm' {name = Lude.Nothing}

-- | The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alarm (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Alarm -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Alarm)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Alarm where
  parseJSON =
    Lude.withObject
      "Alarm"
      (\x -> Alarm' Lude.<$> (x Lude..:? "name"))

instance Lude.ToJSON Alarm where
  toJSON Alarm' {..} =
    Lude.object (Lude.catMaybes [("name" Lude..=) Lude.<$> name])
