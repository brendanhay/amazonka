{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ContentTypeProfileConfig
  ( ContentTypeProfileConfig (..)
  -- * Smart constructor
  , mkContentTypeProfileConfig
  -- * Lenses
  , ctpcForwardWhenContentTypeIsUnknown
  , ctpcContentTypeProfiles
  ) where

import qualified Network.AWS.CloudFront.Types.ContentTypeProfiles as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration for a field-level encryption content type-profile mapping. 
--
-- /See:/ 'mkContentTypeProfileConfig' smart constructor.
data ContentTypeProfileConfig = ContentTypeProfileConfig'
  { forwardWhenContentTypeIsUnknown :: Core.Bool
    -- ^ The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown. 
  , contentTypeProfiles :: Core.Maybe Types.ContentTypeProfiles
    -- ^ The configuration for a field-level encryption content type-profile. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContentTypeProfileConfig' value with any optional fields omitted.
mkContentTypeProfileConfig
    :: Core.Bool -- ^ 'forwardWhenContentTypeIsUnknown'
    -> ContentTypeProfileConfig
mkContentTypeProfileConfig forwardWhenContentTypeIsUnknown
  = ContentTypeProfileConfig'{forwardWhenContentTypeIsUnknown,
                              contentTypeProfiles = Core.Nothing}

-- | The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown. 
--
-- /Note:/ Consider using 'forwardWhenContentTypeIsUnknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpcForwardWhenContentTypeIsUnknown :: Lens.Lens' ContentTypeProfileConfig Core.Bool
ctpcForwardWhenContentTypeIsUnknown = Lens.field @"forwardWhenContentTypeIsUnknown"
{-# INLINEABLE ctpcForwardWhenContentTypeIsUnknown #-}
{-# DEPRECATED forwardWhenContentTypeIsUnknown "Use generic-lens or generic-optics with 'forwardWhenContentTypeIsUnknown' instead"  #-}

-- | The configuration for a field-level encryption content type-profile. 
--
-- /Note:/ Consider using 'contentTypeProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpcContentTypeProfiles :: Lens.Lens' ContentTypeProfileConfig (Core.Maybe Types.ContentTypeProfiles)
ctpcContentTypeProfiles = Lens.field @"contentTypeProfiles"
{-# INLINEABLE ctpcContentTypeProfiles #-}
{-# DEPRECATED contentTypeProfiles "Use generic-lens or generic-optics with 'contentTypeProfiles' instead"  #-}

instance Core.ToXML ContentTypeProfileConfig where
        toXML ContentTypeProfileConfig{..}
          = Core.toXMLElement "ForwardWhenContentTypeIsUnknown"
              forwardWhenContentTypeIsUnknown
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ContentTypeProfiles")
                contentTypeProfiles

instance Core.FromXML ContentTypeProfileConfig where
        parseXML x
          = ContentTypeProfileConfig' Core.<$>
              (x Core..@ "ForwardWhenContentTypeIsUnknown") Core.<*>
                x Core..@? "ContentTypeProfiles"
