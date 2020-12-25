{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TeletextDestinationSettings
  ( TeletextDestinationSettings (..),

    -- * Smart constructor
    mkTeletextDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Teletext Destination Settings
--
-- /See:/ 'mkTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TeletextDestinationSettings' value with any optional fields omitted.
mkTeletextDestinationSettings ::
  TeletextDestinationSettings
mkTeletextDestinationSettings = TeletextDestinationSettings'

instance Core.FromJSON TeletextDestinationSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON TeletextDestinationSettings where
  parseJSON =
    Core.withObject "TeletextDestinationSettings" Core.$
      \x -> Core.pure TeletextDestinationSettings'
