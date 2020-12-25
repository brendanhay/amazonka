{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
  ( RegisterInstanceTagAttributeRequest (..),

    -- * Smart constructor
    mkRegisterInstanceTagAttributeRequest,

    -- * Lenses
    ritarIncludeAllTagsOfInstance,
    ritarInstanceTagKeys,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the tag keys to register for the current Region. You can either specify individual tag keys or register all tag keys in the current Region. You must specify either @IncludeAllTagsOfInstance@ or @InstanceTagKeys@ in the request
--
-- /See:/ 'mkRegisterInstanceTagAttributeRequest' smart constructor.
data RegisterInstanceTagAttributeRequest = RegisterInstanceTagAttributeRequest'
  { -- | Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
    includeAllTagsOfInstance :: Core.Maybe Core.Bool,
    -- | The tag keys to register.
    instanceTagKeys :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterInstanceTagAttributeRequest' value with any optional fields omitted.
mkRegisterInstanceTagAttributeRequest ::
  RegisterInstanceTagAttributeRequest
mkRegisterInstanceTagAttributeRequest =
  RegisterInstanceTagAttributeRequest'
    { includeAllTagsOfInstance =
        Core.Nothing,
      instanceTagKeys = Core.Nothing
    }

-- | Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarIncludeAllTagsOfInstance :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe Core.Bool)
ritarIncludeAllTagsOfInstance = Lens.field @"includeAllTagsOfInstance"
{-# DEPRECATED ritarIncludeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead." #-}

-- | The tag keys to register.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarInstanceTagKeys :: Lens.Lens' RegisterInstanceTagAttributeRequest (Core.Maybe [Types.String])
ritarInstanceTagKeys = Lens.field @"instanceTagKeys"
{-# DEPRECATED ritarInstanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead." #-}
