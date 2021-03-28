{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AribDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AribDestinationSettings
  ( AribDestinationSettings (..)
  -- * Smart constructor
  , mkAribDestinationSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Arib Destination Settings
--
-- /See:/ 'mkAribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AribDestinationSettings' value with any optional fields omitted.
mkAribDestinationSettings
    :: AribDestinationSettings
mkAribDestinationSettings = AribDestinationSettings'

instance Core.FromJSON AribDestinationSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON AribDestinationSettings where
        parseJSON
          = Core.withObject "AribDestinationSettings" Core.$
              \ x -> Core.pure AribDestinationSettings'
