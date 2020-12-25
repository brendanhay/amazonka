{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfileConfig
  ( QueryArgProfileConfig (..),

    -- * Smart constructor
    mkQueryArgProfileConfig,

    -- * Lenses
    qapcForwardWhenQueryArgProfileIsUnknown,
    qapcQueryArgProfiles,
  )
where

import qualified Network.AWS.CloudFront.Types.QueryArgProfiles as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration for query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { -- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
    forwardWhenQueryArgProfileIsUnknown :: Core.Bool,
    -- | Profiles specified for query argument-profile mapping for field-level encryption.
    queryArgProfiles :: Core.Maybe Types.QueryArgProfiles
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryArgProfileConfig' value with any optional fields omitted.
mkQueryArgProfileConfig ::
  -- | 'forwardWhenQueryArgProfileIsUnknown'
  Core.Bool ->
  QueryArgProfileConfig
mkQueryArgProfileConfig forwardWhenQueryArgProfileIsUnknown =
  QueryArgProfileConfig'
    { forwardWhenQueryArgProfileIsUnknown,
      queryArgProfiles = Core.Nothing
    }

-- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
--
-- /Note:/ Consider using 'forwardWhenQueryArgProfileIsUnknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcForwardWhenQueryArgProfileIsUnknown :: Lens.Lens' QueryArgProfileConfig Core.Bool
qapcForwardWhenQueryArgProfileIsUnknown = Lens.field @"forwardWhenQueryArgProfileIsUnknown"
{-# DEPRECATED qapcForwardWhenQueryArgProfileIsUnknown "Use generic-lens or generic-optics with 'forwardWhenQueryArgProfileIsUnknown' instead." #-}

-- | Profiles specified for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'queryArgProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcQueryArgProfiles :: Lens.Lens' QueryArgProfileConfig (Core.Maybe Types.QueryArgProfiles)
qapcQueryArgProfiles = Lens.field @"queryArgProfiles"
{-# DEPRECATED qapcQueryArgProfiles "Use generic-lens or generic-optics with 'queryArgProfiles' instead." #-}

instance Core.ToXML QueryArgProfileConfig where
  toXML QueryArgProfileConfig {..} =
    Core.toXMLNode
      "ForwardWhenQueryArgProfileIsUnknown"
      forwardWhenQueryArgProfileIsUnknown
      Core.<> Core.toXMLNode "QueryArgProfiles" Core.<$> queryArgProfiles

instance Core.FromXML QueryArgProfileConfig where
  parseXML x =
    QueryArgProfileConfig'
      Core.<$> (x Core..@ "ForwardWhenQueryArgProfileIsUnknown")
      Core.<*> (x Core..@? "QueryArgProfiles")
