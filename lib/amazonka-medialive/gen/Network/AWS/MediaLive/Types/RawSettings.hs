{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RawSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.RawSettings
  ( RawSettings (..)
  -- * Smart constructor
  , mkRawSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Raw Settings
--
-- /See:/ 'mkRawSettings' smart constructor.
data RawSettings = RawSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RawSettings' value with any optional fields omitted.
mkRawSettings
    :: RawSettings
mkRawSettings = RawSettings'

instance Core.FromJSON RawSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON RawSettings where
        parseJSON
          = Core.withObject "RawSettings" Core.$
              \ x -> Core.pure RawSettings'
