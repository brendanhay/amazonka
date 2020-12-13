{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileAssociation
  ( IAMInstanceProfileAssociation (..),

    -- * Smart constructor
    mkIAMInstanceProfileAssociation,

    -- * Lenses
    iapaAssociationId,
    iapaInstanceId,
    iapaState,
    iapaIAMInstanceProfile,
    iapaTimestamp,
  )
where

import Network.AWS.EC2.Types.IAMInstanceProfile
import Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between an IAM instance profile and an instance.
--
-- /See:/ 'mkIAMInstanceProfileAssociation' smart constructor.
data IAMInstanceProfileAssociation = IAMInstanceProfileAssociation'
  { -- | The ID of the association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The state of the association.
    state :: Lude.Maybe IAMInstanceProfileAssociationState,
    -- | The IAM instance profile.
    iamInstanceProfile :: Lude.Maybe IAMInstanceProfile,
    -- | The time the IAM instance profile was associated with the instance.
    timestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IAMInstanceProfileAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the association.
-- * 'instanceId' - The ID of the instance.
-- * 'state' - The state of the association.
-- * 'iamInstanceProfile' - The IAM instance profile.
-- * 'timestamp' - The time the IAM instance profile was associated with the instance.
mkIAMInstanceProfileAssociation ::
  IAMInstanceProfileAssociation
mkIAMInstanceProfileAssociation =
  IAMInstanceProfileAssociation'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      state = Lude.Nothing,
      iamInstanceProfile = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The ID of the association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapaAssociationId :: Lens.Lens' IAMInstanceProfileAssociation (Lude.Maybe Lude.Text)
iapaAssociationId = Lens.lens (associationId :: IAMInstanceProfileAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: IAMInstanceProfileAssociation)
{-# DEPRECATED iapaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapaInstanceId :: Lens.Lens' IAMInstanceProfileAssociation (Lude.Maybe Lude.Text)
iapaInstanceId = Lens.lens (instanceId :: IAMInstanceProfileAssociation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: IAMInstanceProfileAssociation)
{-# DEPRECATED iapaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapaState :: Lens.Lens' IAMInstanceProfileAssociation (Lude.Maybe IAMInstanceProfileAssociationState)
iapaState = Lens.lens (state :: IAMInstanceProfileAssociation -> Lude.Maybe IAMInstanceProfileAssociationState) (\s a -> s {state = a} :: IAMInstanceProfileAssociation)
{-# DEPRECATED iapaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapaIAMInstanceProfile :: Lens.Lens' IAMInstanceProfileAssociation (Lude.Maybe IAMInstanceProfile)
iapaIAMInstanceProfile = Lens.lens (iamInstanceProfile :: IAMInstanceProfileAssociation -> Lude.Maybe IAMInstanceProfile) (\s a -> s {iamInstanceProfile = a} :: IAMInstanceProfileAssociation)
{-# DEPRECATED iapaIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The time the IAM instance profile was associated with the instance.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapaTimestamp :: Lens.Lens' IAMInstanceProfileAssociation (Lude.Maybe Lude.DateTime)
iapaTimestamp = Lens.lens (timestamp :: IAMInstanceProfileAssociation -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: IAMInstanceProfileAssociation)
{-# DEPRECATED iapaTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML IAMInstanceProfileAssociation where
  parseXML x =
    IAMInstanceProfileAssociation'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "iamInstanceProfile")
      Lude.<*> (x Lude..@? "timestamp")
