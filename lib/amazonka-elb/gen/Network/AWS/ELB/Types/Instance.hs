{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iInstanceId,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ID of an EC2 instance.
--
-- /See:/ 'mkInstance' smart constructor.
newtype Instance = Instance'
  { -- | The instance ID.
    instanceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
mkInstance ::
  Instance
mkInstance = Instance' {instanceId = Lude.Nothing}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceId = Lens.lens (instanceId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Instance)
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.FromXML Instance where
  parseXML x = Instance' Lude.<$> (x Lude..@? "InstanceId")

instance Lude.ToQuery Instance where
  toQuery Instance' {..} =
    Lude.mconcat ["InstanceId" Lude.=: instanceId]
