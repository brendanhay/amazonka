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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the tag keys to register for the current Region. You can either specify individual tag keys or register all tag keys in the current Region. You must specify either @IncludeAllTagsOfInstance@ or @InstanceTagKeys@ in the request
--
-- /See:/ 'mkRegisterInstanceTagAttributeRequest' smart constructor.
data RegisterInstanceTagAttributeRequest = RegisterInstanceTagAttributeRequest'
  { includeAllTagsOfInstance ::
      Lude.Maybe
        Lude.Bool,
    instanceTagKeys ::
      Lude.Maybe
        [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstanceTagAttributeRequest' with the minimum fields required to make a request.
--
-- * 'includeAllTagsOfInstance' - Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
-- * 'instanceTagKeys' - The tag keys to register.
mkRegisterInstanceTagAttributeRequest ::
  RegisterInstanceTagAttributeRequest
mkRegisterInstanceTagAttributeRequest =
  RegisterInstanceTagAttributeRequest'
    { includeAllTagsOfInstance =
        Lude.Nothing,
      instanceTagKeys = Lude.Nothing
    }

-- | Indicates whether to register all tag keys in the current Region. Specify @true@ to register all tag keys.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarIncludeAllTagsOfInstance :: Lens.Lens' RegisterInstanceTagAttributeRequest (Lude.Maybe Lude.Bool)
ritarIncludeAllTagsOfInstance = Lens.lens (includeAllTagsOfInstance :: RegisterInstanceTagAttributeRequest -> Lude.Maybe Lude.Bool) (\s a -> s {includeAllTagsOfInstance = a} :: RegisterInstanceTagAttributeRequest)
{-# DEPRECATED ritarIncludeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead." #-}

-- | The tag keys to register.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritarInstanceTagKeys :: Lens.Lens' RegisterInstanceTagAttributeRequest (Lude.Maybe [Lude.Text])
ritarInstanceTagKeys = Lens.lens (instanceTagKeys :: RegisterInstanceTagAttributeRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceTagKeys = a} :: RegisterInstanceTagAttributeRequest)
{-# DEPRECATED ritarInstanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead." #-}

instance Lude.ToQuery RegisterInstanceTagAttributeRequest where
  toQuery RegisterInstanceTagAttributeRequest' {..} =
    Lude.mconcat
      [ "IncludeAllTagsOfInstance" Lude.=: includeAllTagsOfInstance,
        Lude.toQuery
          (Lude.toQueryList "InstanceTagKey" Lude.<$> instanceTagKeys)
      ]
