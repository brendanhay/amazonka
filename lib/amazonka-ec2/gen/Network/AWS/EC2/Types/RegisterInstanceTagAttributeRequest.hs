{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
  ( RegisterInstanceTagAttributeRequest (..)
  -- * Smart constructor
  , mkRegisterInstanceTagAttributeRequest
  -- * Lenses
  , ritarIncludeAllTagsOfInstance
  , ritarInstanceTagKeys
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the tag keys to register for the current Region. You can either specify individual tag keys or register all tag keys in the current Region. You must specify either @IncludeAllTagsOfInstance@ or @InstanceTagKeys@ in the request
--
-- /See:/ 'mkRegisterInstanceTagAttributeRequest' smart constructor.
data RegisterInstanceTagAttributeRequest = RegisterInstanceTagAttributeRequest'
  { includeAllTagsOfInstance :: Core.Maybe Core.Bool
    -- ^ Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
  , instanceTagKeys :: Core.Maybe [Core.Text]
    -- ^ The tag keys to register.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstanceTagAttributeRequest' value with any optional fields omitted.
mkRegisterInstanceTagAttributeRequest
    :: RegisterInstanceTagAttributeRequest
mkRegisterInstanceTagAttributeRequest
  = RegisterInstanceTagAttributeRequest'{includeAllTagsOfInstance =
                                           Core.Nothing,
                                         instanceTagKeys = Core.Nothing}

-- | Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarIncludeAllTagsOfInstance :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe Core.Bool)
ritarIncludeAllTagsOfInstance = Lens.field @"includeAllTagsOfInstance"
{-# INLINEABLE ritarIncludeAllTagsOfInstance #-}
{-# DEPRECATED includeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead"  #-}

-- | The tag keys to register.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarInstanceTagKeys :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe [Core.Text])
ritarInstanceTagKeys = Lens.field @"instanceTagKeys"
{-# INLINEABLE ritarInstanceTagKeys #-}
{-# DEPRECATED instanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead"  #-}

instance Core.ToQuery RegisterInstanceTagAttributeRequest where
        toQuery RegisterInstanceTagAttributeRequest{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "IncludeAllTagsOfInstance")
              includeAllTagsOfInstance
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "InstanceTagKey")
                instanceTagKeys
