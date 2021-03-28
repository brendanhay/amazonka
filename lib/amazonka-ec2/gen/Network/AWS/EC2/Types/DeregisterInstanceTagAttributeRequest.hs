{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
  ( DeregisterInstanceTagAttributeRequest (..)
  -- * Smart constructor
  , mkDeregisterInstanceTagAttributeRequest
  -- * Lenses
  , ditarIncludeAllTagsOfInstance
  , ditarInstanceTagKeys
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the tag keys to deregister for the current Region. You can either specify individual tag keys or deregister all tag keys in the current Region. You must specify either @IncludeAllTagsOfInstance@ or @InstanceTagKeys@ in the request
--
-- /See:/ 'mkDeregisterInstanceTagAttributeRequest' smart constructor.
data DeregisterInstanceTagAttributeRequest = DeregisterInstanceTagAttributeRequest'
  { includeAllTagsOfInstance :: Core.Maybe Core.Bool
    -- ^ Indicates whether to deregister all tag keys in the current Region. Specify @false@ to deregister all tag keys.
  , instanceTagKeys :: Core.Maybe [Core.Text]
    -- ^ Information about the tag keys to deregister.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstanceTagAttributeRequest' value with any optional fields omitted.
mkDeregisterInstanceTagAttributeRequest
    :: DeregisterInstanceTagAttributeRequest
mkDeregisterInstanceTagAttributeRequest
  = DeregisterInstanceTagAttributeRequest'{includeAllTagsOfInstance =
                                             Core.Nothing,
                                           instanceTagKeys = Core.Nothing}

-- | Indicates whether to deregister all tag keys in the current Region. Specify @false@ to deregister all tag keys.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditarIncludeAllTagsOfInstance :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Core.Maybe Core.Bool)
ditarIncludeAllTagsOfInstance = Lens.field @"includeAllTagsOfInstance"
{-# INLINEABLE ditarIncludeAllTagsOfInstance #-}
{-# DEPRECATED includeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead"  #-}

-- | Information about the tag keys to deregister.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditarInstanceTagKeys :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Core.Maybe [Core.Text])
ditarInstanceTagKeys = Lens.field @"instanceTagKeys"
{-# INLINEABLE ditarInstanceTagKeys #-}
{-# DEPRECATED instanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead"  #-}

instance Core.ToQuery DeregisterInstanceTagAttributeRequest where
        toQuery DeregisterInstanceTagAttributeRequest{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "IncludeAllTagsOfInstance")
              includeAllTagsOfInstance
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "InstanceTagKey")
                instanceTagKeys
