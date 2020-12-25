{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AribSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribSourceSettings
  ( AribSourceSettings (..),

    -- * Smart constructor
    mkAribSourceSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Arib Source Settings
--
-- /See:/ 'mkAribSourceSettings' smart constructor.
data AribSourceSettings = AribSourceSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AribSourceSettings' value with any optional fields omitted.
mkAribSourceSettings ::
  AribSourceSettings
mkAribSourceSettings = AribSourceSettings'

instance Core.FromJSON AribSourceSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON AribSourceSettings where
  parseJSON =
    Core.withObject "AribSourceSettings" Core.$
      \x -> Core.pure AribSourceSettings'
