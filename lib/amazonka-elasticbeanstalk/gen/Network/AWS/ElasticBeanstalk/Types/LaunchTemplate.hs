{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
  ( LaunchTemplate (..),

    -- * Smart constructor
    mkLaunchTemplate,

    -- * Lenses
    ltId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon EC2 launch template.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
newtype LaunchTemplate = LaunchTemplate'
  { id ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the launch template.
mkLaunchTemplate ::
  LaunchTemplate
mkLaunchTemplate = LaunchTemplate' {id = Lude.Nothing}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltId :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Text)
ltId = Lens.lens (id :: LaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: LaunchTemplate)
{-# DEPRECATED ltId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML LaunchTemplate where
  parseXML x = LaunchTemplate' Lude.<$> (x Lude..@? "Id")
