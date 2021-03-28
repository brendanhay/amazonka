{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.UserData
  ( UserData (..)
  -- * Smart constructor
  , mkUserData
  -- * Lenses
  , udEmail
  , udEnrollmentId
  , udEnrollmentStatus
  , udFirstName
  , udLastName
  , udUserArn
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.Email as Types
import qualified Network.AWS.AlexaBusiness.Types.EnrollmentId as Types
import qualified Network.AWS.AlexaBusiness.Types.EnrollmentStatus as Types
import qualified Network.AWS.AlexaBusiness.Types.FirstName as Types
import qualified Network.AWS.AlexaBusiness.Types.User_LastName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information related to a user.
--
-- /See:/ 'mkUserData' smart constructor.
data UserData = UserData'
  { email :: Core.Maybe Types.Email
    -- ^ The email of a user.
  , enrollmentId :: Core.Maybe Types.EnrollmentId
    -- ^ The enrollment ARN of a user.
  , enrollmentStatus :: Core.Maybe Types.EnrollmentStatus
    -- ^ The enrollment status of a user.
  , firstName :: Core.Maybe Types.FirstName
    -- ^ The first name of a user.
  , lastName :: Core.Maybe Types.User_LastName
    -- ^ The last name of a user.
  , userArn :: Core.Maybe Types.Arn
    -- ^ The ARN of a user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserData' value with any optional fields omitted.
mkUserData
    :: UserData
mkUserData
  = UserData'{email = Core.Nothing, enrollmentId = Core.Nothing,
              enrollmentStatus = Core.Nothing, firstName = Core.Nothing,
              lastName = Core.Nothing, userArn = Core.Nothing}

-- | The email of a user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEmail :: Lens.Lens' UserData (Core.Maybe Types.Email)
udEmail = Lens.field @"email"
{-# INLINEABLE udEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The enrollment ARN of a user.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnrollmentId :: Lens.Lens' UserData (Core.Maybe Types.EnrollmentId)
udEnrollmentId = Lens.field @"enrollmentId"
{-# INLINEABLE udEnrollmentId #-}
{-# DEPRECATED enrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead"  #-}

-- | The enrollment status of a user.
--
-- /Note:/ Consider using 'enrollmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnrollmentStatus :: Lens.Lens' UserData (Core.Maybe Types.EnrollmentStatus)
udEnrollmentStatus = Lens.field @"enrollmentStatus"
{-# INLINEABLE udEnrollmentStatus #-}
{-# DEPRECATED enrollmentStatus "Use generic-lens or generic-optics with 'enrollmentStatus' instead"  #-}

-- | The first name of a user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFirstName :: Lens.Lens' UserData (Core.Maybe Types.FirstName)
udFirstName = Lens.field @"firstName"
{-# INLINEABLE udFirstName #-}
{-# DEPRECATED firstName "Use generic-lens or generic-optics with 'firstName' instead"  #-}

-- | The last name of a user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLastName :: Lens.Lens' UserData (Core.Maybe Types.User_LastName)
udLastName = Lens.field @"lastName"
{-# INLINEABLE udLastName #-}
{-# DEPRECATED lastName "Use generic-lens or generic-optics with 'lastName' instead"  #-}

-- | The ARN of a user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserArn :: Lens.Lens' UserData (Core.Maybe Types.Arn)
udUserArn = Lens.field @"userArn"
{-# INLINEABLE udUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.FromJSON UserData where
        parseJSON
          = Core.withObject "UserData" Core.$
              \ x ->
                UserData' Core.<$>
                  (x Core..:? "Email") Core.<*> x Core..:? "EnrollmentId" Core.<*>
                    x Core..:? "EnrollmentStatus"
                    Core.<*> x Core..:? "FirstName"
                    Core.<*> x Core..:? "LastName"
                    Core.<*> x Core..:? "UserArn"
