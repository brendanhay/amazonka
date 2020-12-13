{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of an Amazon EC2 instance.
--
-- /See:/ 'mkInstance' smart constructor.
newtype Instance = Instance'
  { -- | The ID of the Amazon EC2 instance.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the Amazon EC2 instance.
mkInstance ::
  Instance
mkInstance = Instance' {id = Lude.Nothing}

-- | The ID of the Amazon EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iId = Lens.lens (id :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Instance)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML Instance where
  parseXML x = Instance' Lude.<$> (x Lude..@? "Id")
