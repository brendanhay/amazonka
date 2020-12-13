{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostInstance
  ( HostInstance (..),

    -- * Smart constructor
    mkHostInstance,

    -- * Lenses
    hiInstanceId,
    hiInstanceType,
    hiOwnerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance running on a Dedicated Host.
--
-- /See:/ 'mkHostInstance' smart constructor.
data HostInstance = HostInstance'
  { -- | The ID of instance that is running on the Dedicated Host.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The instance type (for example, @m3.medium@ ) of the running instance.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS account that owns the instance.
    ownerId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of instance that is running on the Dedicated Host.
-- * 'instanceType' - The instance type (for example, @m3.medium@ ) of the running instance.
-- * 'ownerId' - The ID of the AWS account that owns the instance.
mkHostInstance ::
  HostInstance
mkHostInstance =
  HostInstance'
    { instanceId = Lude.Nothing,
      instanceType = Lude.Nothing,
      ownerId = Lude.Nothing
    }

-- | The ID of instance that is running on the Dedicated Host.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiInstanceId :: Lens.Lens' HostInstance (Lude.Maybe Lude.Text)
hiInstanceId = Lens.lens (instanceId :: HostInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: HostInstance)
{-# DEPRECATED hiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance type (for example, @m3.medium@ ) of the running instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiInstanceType :: Lens.Lens' HostInstance (Lude.Maybe Lude.Text)
hiInstanceType = Lens.lens (instanceType :: HostInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: HostInstance)
{-# DEPRECATED hiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the AWS account that owns the instance.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiOwnerId :: Lens.Lens' HostInstance (Lude.Maybe Lude.Text)
hiOwnerId = Lens.lens (ownerId :: HostInstance -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: HostInstance)
{-# DEPRECATED hiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

instance Lude.FromXML HostInstance where
  parseXML x =
    HostInstance'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "ownerId")
