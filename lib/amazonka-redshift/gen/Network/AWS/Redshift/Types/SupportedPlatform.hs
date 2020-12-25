{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedPlatform
  ( SupportedPlatform (..),

    -- * Smart constructor
    mkSupportedPlatform,

    -- * Lenses
    spName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | A list of supported platforms for orderable clusters.
--
-- /See:/ 'mkSupportedPlatform' smart constructor.
newtype SupportedPlatform = SupportedPlatform'
  { -- |
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SupportedPlatform' value with any optional fields omitted.
mkSupportedPlatform ::
  SupportedPlatform
mkSupportedPlatform = SupportedPlatform' {name = Core.Nothing}

-- |
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' SupportedPlatform (Core.Maybe Types.String)
spName = Lens.field @"name"
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML SupportedPlatform where
  parseXML x = SupportedPlatform' Core.<$> (x Core..@? "Name")
