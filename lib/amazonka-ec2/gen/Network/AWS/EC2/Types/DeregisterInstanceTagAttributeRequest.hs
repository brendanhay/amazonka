-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
  ( DeregisterInstanceTagAttributeRequest (..),

    -- * Smart constructor
    mkDeregisterInstanceTagAttributeRequest,

    -- * Lenses
    ditarIncludeAllTagsOfInstance,
    ditarInstanceTagKeys,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the tag keys to deregister for the current Region. You can either specify individual tag keys or deregister all tag keys in the current Region. You must specify either @IncludeAllTagsOfInstance@ or @InstanceTagKeys@ in the request
--
-- /See:/ 'mkDeregisterInstanceTagAttributeRequest' smart constructor.
data DeregisterInstanceTagAttributeRequest = DeregisterInstanceTagAttributeRequest'
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

-- | Creates a value of 'DeregisterInstanceTagAttributeRequest' with the minimum fields required to make a request.
--
-- * 'includeAllTagsOfInstance' - Indicates whether to deregister all tag keys in the current Region. Specify @false@ to deregister all tag keys.
-- * 'instanceTagKeys' - Information about the tag keys to deregister.
mkDeregisterInstanceTagAttributeRequest ::
  DeregisterInstanceTagAttributeRequest
mkDeregisterInstanceTagAttributeRequest =
  DeregisterInstanceTagAttributeRequest'
    { includeAllTagsOfInstance =
        Lude.Nothing,
      instanceTagKeys = Lude.Nothing
    }

-- | Indicates whether to deregister all tag keys in the current Region. Specify @false@ to deregister all tag keys.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditarIncludeAllTagsOfInstance :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Lude.Maybe Lude.Bool)
ditarIncludeAllTagsOfInstance = Lens.lens (includeAllTagsOfInstance :: DeregisterInstanceTagAttributeRequest -> Lude.Maybe Lude.Bool) (\s a -> s {includeAllTagsOfInstance = a} :: DeregisterInstanceTagAttributeRequest)
{-# DEPRECATED ditarIncludeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead." #-}

-- | Information about the tag keys to deregister.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditarInstanceTagKeys :: Lens.Lens' DeregisterInstanceTagAttributeRequest (Lude.Maybe [Lude.Text])
ditarInstanceTagKeys = Lens.lens (instanceTagKeys :: DeregisterInstanceTagAttributeRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceTagKeys = a} :: DeregisterInstanceTagAttributeRequest)
{-# DEPRECATED ditarInstanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead." #-}

instance Lude.ToQuery DeregisterInstanceTagAttributeRequest where
  toQuery DeregisterInstanceTagAttributeRequest' {..} =
    Lude.mconcat
      [ "IncludeAllTagsOfInstance" Lude.=: includeAllTagsOfInstance,
        Lude.toQuery
          (Lude.toQueryList "InstanceTagKey" Lude.<$> instanceTagKeys)
      ]
