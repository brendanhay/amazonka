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
import qualified Network.AWS.Prelude as Lude

-- | Settings to configure an action so that it occurs as soon as possible.
--
-- /See:/ 'mkImmediateModeScheduleActionStartSettings' smart constructor.
data ImmediateModeScheduleActionStartSettings = ImmediateModeScheduleActionStartSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImmediateModeScheduleActionStartSettings' with the minimum fields required to make a request.
mkImmediateModeScheduleActionStartSettings ::
  ImmediateModeScheduleActionStartSettings
mkImmediateModeScheduleActionStartSettings =
  ImmediateModeScheduleActionStartSettings'

instance Lude.FromJSON ImmediateModeScheduleActionStartSettings where
  parseJSON =
    Lude.withObject
      "ImmediateModeScheduleActionStartSettings"
      (\x -> Lude.pure ImmediateModeScheduleActionStartSettings')

instance Lude.ToJSON ImmediateModeScheduleActionStartSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
