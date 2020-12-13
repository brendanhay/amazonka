{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTagNotificationAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTagNotificationAttribute
  ( InstanceTagNotificationAttribute (..),

    -- * Smart constructor
    mkInstanceTagNotificationAttribute,

    -- * Lenses
    itnaIncludeAllTagsOfInstance,
    itnaInstanceTagKeys,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the registered tag keys for the current Region.
--
-- /See:/ 'mkInstanceTagNotificationAttribute' smart constructor.
data InstanceTagNotificationAttribute = InstanceTagNotificationAttribute'
  { -- | Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
    includeAllTagsOfInstance :: Lude.Maybe Lude.Bool,
    -- | The registered tag keys.
    instanceTagKeys :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTagNotificationAttribute' with the minimum fields required to make a request.
--
-- * 'includeAllTagsOfInstance' - Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
-- * 'instanceTagKeys' - The registered tag keys.
mkInstanceTagNotificationAttribute ::
  InstanceTagNotificationAttribute
mkInstanceTagNotificationAttribute =
  InstanceTagNotificationAttribute'
    { includeAllTagsOfInstance =
        Lude.Nothing,
      instanceTagKeys = Lude.Nothing
    }

-- | Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itnaIncludeAllTagsOfInstance :: Lens.Lens' InstanceTagNotificationAttribute (Lude.Maybe Lude.Bool)
itnaIncludeAllTagsOfInstance = Lens.lens (includeAllTagsOfInstance :: InstanceTagNotificationAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {includeAllTagsOfInstance = a} :: InstanceTagNotificationAttribute)
{-# DEPRECATED itnaIncludeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead." #-}

-- | The registered tag keys.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itnaInstanceTagKeys :: Lens.Lens' InstanceTagNotificationAttribute (Lude.Maybe [Lude.Text])
itnaInstanceTagKeys = Lens.lens (instanceTagKeys :: InstanceTagNotificationAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceTagKeys = a} :: InstanceTagNotificationAttribute)
{-# DEPRECATED itnaInstanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead." #-}

instance Lude.FromXML InstanceTagNotificationAttribute where
  parseXML x =
    InstanceTagNotificationAttribute'
      Lude.<$> (x Lude..@? "includeAllTagsOfInstance")
      Lude.<*> ( x Lude..@? "instanceTagKeySet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
