{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.QueryArgProfileConfig
  ( QueryArgProfileConfig (..)
  -- * Smart constructor
  , mkQueryArgProfileConfig
  -- * Lenses
  , qapcForwardWhenQueryArgProfileIsUnknown
  , qapcQueryArgProfiles
  ) where

import qualified Network.AWS.CloudFront.Types.QueryArgProfiles as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration for query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { forwardWhenQueryArgProfileIsUnknown :: Core.Bool
    -- ^ Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
  , queryArgProfiles :: Core.Maybe Types.QueryArgProfiles
    -- ^ Profiles specified for query argument-profile mapping for field-level encryption.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryArgProfileConfig' value with any optional fields omitted.
mkQueryArgProfileConfig
    :: Core.Bool -- ^ 'forwardWhenQueryArgProfileIsUnknown'
    -> QueryArgProfileConfig
mkQueryArgProfileConfig forwardWhenQueryArgProfileIsUnknown
  = QueryArgProfileConfig'{forwardWhenQueryArgProfileIsUnknown,
                           queryArgProfiles = Core.Nothing}

-- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
--
-- /Note:/ Consider using 'forwardWhenQueryArgProfileIsUnknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcForwardWhenQueryArgProfileIsUnknown :: Lens.Lens' QueryArgProfileConfig Core.Bool
qapcForwardWhenQueryArgProfileIsUnknown = Lens.field @"forwardWhenQueryArgProfileIsUnknown"
{-# INLINEABLE qapcForwardWhenQueryArgProfileIsUnknown #-}
{-# DEPRECATED forwardWhenQueryArgProfileIsUnknown "Use generic-lens or generic-optics with 'forwardWhenQueryArgProfileIsUnknown' instead"  #-}

-- | Profiles specified for query argument-profile mapping for field-level encryption.
--
-- /Note:/ Consider using 'queryArgProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapcQueryArgProfiles :: Lens.Lens' QueryArgProfileConfig (Core.Maybe Types.QueryArgProfiles)
qapcQueryArgProfiles = Lens.field @"queryArgProfiles"
{-# INLINEABLE qapcQueryArgProfiles #-}
{-# DEPRECATED queryArgProfiles "Use generic-lens or generic-optics with 'queryArgProfiles' instead"  #-}

instance Core.ToXML QueryArgProfileConfig where
        toXML QueryArgProfileConfig{..}
          = Core.toXMLElement "ForwardWhenQueryArgProfileIsUnknown"
              forwardWhenQueryArgProfileIsUnknown
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "QueryArgProfiles")
                queryArgProfiles

instance Core.FromXML QueryArgProfileConfig where
        parseXML x
          = QueryArgProfileConfig' Core.<$>
              (x Core..@ "ForwardWhenQueryArgProfileIsUnknown") Core.<*>
                x Core..@? "QueryArgProfiles"
