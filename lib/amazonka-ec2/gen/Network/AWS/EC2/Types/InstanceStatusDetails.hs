{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusDetails
  ( InstanceStatusDetails (..),

    -- * Smart constructor
    mkInstanceStatusDetails,

    -- * Lenses
    isdStatus,
    isdImpairedSince,
    isdName,
  )
where

import Network.AWS.EC2.Types.StatusName
import Network.AWS.EC2.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instance status.
--
-- /See:/ 'mkInstanceStatusDetails' smart constructor.
data InstanceStatusDetails = InstanceStatusDetails'
  { -- | The status.
    status :: Lude.Maybe StatusType,
    -- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
    impairedSince :: Lude.Maybe Lude.DateTime,
    -- | The type of instance status.
    name :: Lude.Maybe StatusName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStatusDetails' with the minimum fields required to make a request.
--
-- * 'status' - The status.
-- * 'impairedSince' - The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
-- * 'name' - The type of instance status.
mkInstanceStatusDetails ::
  InstanceStatusDetails
mkInstanceStatusDetails =
  InstanceStatusDetails'
    { status = Lude.Nothing,
      impairedSince = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdStatus :: Lens.Lens' InstanceStatusDetails (Lude.Maybe StatusType)
isdStatus = Lens.lens (status :: InstanceStatusDetails -> Lude.Maybe StatusType) (\s a -> s {status = a} :: InstanceStatusDetails)
{-# DEPRECATED isdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
--
-- /Note:/ Consider using 'impairedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdImpairedSince :: Lens.Lens' InstanceStatusDetails (Lude.Maybe Lude.DateTime)
isdImpairedSince = Lens.lens (impairedSince :: InstanceStatusDetails -> Lude.Maybe Lude.DateTime) (\s a -> s {impairedSince = a} :: InstanceStatusDetails)
{-# DEPRECATED isdImpairedSince "Use generic-lens or generic-optics with 'impairedSince' instead." #-}

-- | The type of instance status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdName :: Lens.Lens' InstanceStatusDetails (Lude.Maybe StatusName)
isdName = Lens.lens (name :: InstanceStatusDetails -> Lude.Maybe StatusName) (\s a -> s {name = a} :: InstanceStatusDetails)
{-# DEPRECATED isdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML InstanceStatusDetails where
  parseXML x =
    InstanceStatusDetails'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "impairedSince")
      Lude.<*> (x Lude..@? "name")
