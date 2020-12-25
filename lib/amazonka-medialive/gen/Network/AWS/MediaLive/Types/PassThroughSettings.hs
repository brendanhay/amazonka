{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PassThroughSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PassThroughSettings
  ( PassThroughSettings (..),

    -- * Smart constructor
    mkPassThroughSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Pass Through Settings
--
-- /See:/ 'mkPassThroughSettings' smart constructor.
data PassThroughSettings = PassThroughSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PassThroughSettings' value with any optional fields omitted.
mkPassThroughSettings ::
  PassThroughSettings
mkPassThroughSettings = PassThroughSettings'

instance Core.FromJSON PassThroughSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON PassThroughSettings where
  parseJSON =
    Core.withObject "PassThroughSettings" Core.$
      \x -> Core.pure PassThroughSettings'
