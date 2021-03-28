{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IamInstanceProfileAssociation
  ( IamInstanceProfileAssociation (..)
  -- * Smart constructor
  , mkIamInstanceProfileAssociation
  -- * Lenses
  , iipaAssociationId
  , iipaIamInstanceProfile
  , iipaInstanceId
  , iipaState
  , iipaTimestamp
  ) where

import qualified Network.AWS.EC2.Types.IamInstanceProfile as Types
import qualified Network.AWS.EC2.Types.IamInstanceProfileAssociationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between an IAM instance profile and an instance.
--
-- /See:/ 'mkIamInstanceProfileAssociation' smart constructor.
data IamInstanceProfileAssociation = IamInstanceProfileAssociation'
  { associationId :: Core.Maybe Core.Text
    -- ^ The ID of the association.
  , iamInstanceProfile :: Core.Maybe Types.IamInstanceProfile
    -- ^ The IAM instance profile.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , state :: Core.Maybe Types.IamInstanceProfileAssociationState
    -- ^ The state of the association.
  , timestamp :: Core.Maybe Core.UTCTime
    -- ^ The time the IAM instance profile was associated with the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IamInstanceProfileAssociation' value with any optional fields omitted.
mkIamInstanceProfileAssociation
    :: IamInstanceProfileAssociation
mkIamInstanceProfileAssociation
  = IamInstanceProfileAssociation'{associationId = Core.Nothing,
                                   iamInstanceProfile = Core.Nothing, instanceId = Core.Nothing,
                                   state = Core.Nothing, timestamp = Core.Nothing}

-- | The ID of the association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipaAssociationId :: Lens.Lens' IamInstanceProfileAssociation (Core.Maybe Core.Text)
iipaAssociationId = Lens.field @"associationId"
{-# INLINEABLE iipaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipaIamInstanceProfile :: Lens.Lens' IamInstanceProfileAssociation (Core.Maybe Types.IamInstanceProfile)
iipaIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE iipaIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipaInstanceId :: Lens.Lens' IamInstanceProfileAssociation (Core.Maybe Core.Text)
iipaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iipaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipaState :: Lens.Lens' IamInstanceProfileAssociation (Core.Maybe Types.IamInstanceProfileAssociationState)
iipaState = Lens.field @"state"
{-# INLINEABLE iipaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The time the IAM instance profile was associated with the instance.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipaTimestamp :: Lens.Lens' IamInstanceProfileAssociation (Core.Maybe Core.UTCTime)
iipaTimestamp = Lens.field @"timestamp"
{-# INLINEABLE iipaTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromXML IamInstanceProfileAssociation where
        parseXML x
          = IamInstanceProfileAssociation' Core.<$>
              (x Core..@? "associationId") Core.<*>
                x Core..@? "iamInstanceProfile"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "timestamp"
