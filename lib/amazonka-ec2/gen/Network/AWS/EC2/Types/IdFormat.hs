{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IdFormat
  ( IdFormat (..),

    -- * Smart constructor
    mkIdFormat,

    -- * Lenses
    ifUseLongIds,
    ifDeadline,
    ifResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the ID format for a resource.
--
-- /See:/ 'mkIdFormat' smart constructor.
data IdFormat = IdFormat'
  { useLongIds :: Lude.Maybe Lude.Bool,
    deadline :: Lude.Maybe Lude.DateTime,
    resource :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdFormat' with the minimum fields required to make a request.
--
-- * 'deadline' - The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
-- * 'resource' - The type of resource.
-- * 'useLongIds' - Indicates whether longer IDs (17-character IDs) are enabled for the resource.
mkIdFormat ::
  IdFormat
mkIdFormat =
  IdFormat'
    { useLongIds = Lude.Nothing,
      deadline = Lude.Nothing,
      resource = Lude.Nothing
    }

-- | Indicates whether longer IDs (17-character IDs) are enabled for the resource.
--
-- /Note:/ Consider using 'useLongIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifUseLongIds :: Lens.Lens' IdFormat (Lude.Maybe Lude.Bool)
ifUseLongIds = Lens.lens (useLongIds :: IdFormat -> Lude.Maybe Lude.Bool) (\s a -> s {useLongIds = a} :: IdFormat)
{-# DEPRECATED ifUseLongIds "Use generic-lens or generic-optics with 'useLongIds' instead." #-}

-- | The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
--
-- /Note:/ Consider using 'deadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDeadline :: Lens.Lens' IdFormat (Lude.Maybe Lude.DateTime)
ifDeadline = Lens.lens (deadline :: IdFormat -> Lude.Maybe Lude.DateTime) (\s a -> s {deadline = a} :: IdFormat)
{-# DEPRECATED ifDeadline "Use generic-lens or generic-optics with 'deadline' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifResource :: Lens.Lens' IdFormat (Lude.Maybe Lude.Text)
ifResource = Lens.lens (resource :: IdFormat -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: IdFormat)
{-# DEPRECATED ifResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromXML IdFormat where
  parseXML x =
    IdFormat'
      Lude.<$> (x Lude..@? "useLongIds")
      Lude.<*> (x Lude..@? "deadline")
      Lude.<*> (x Lude..@? "resource")
