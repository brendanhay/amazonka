{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserContext
  ( UserContext (..),

    -- * Smart constructor
    mkUserContext,

    -- * Lenses
    ucDomainId,
    ucUserProfileArn,
    ucUserProfileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.String as Types

-- | Information about the user who created or modified an experiment, trial, or trial component.
--
-- /See:/ 'mkUserContext' smart constructor.
data UserContext = UserContext'
  { -- | The domain associated with the user.
    domainId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the user's profile.
    userProfileArn :: Core.Maybe Types.String,
    -- | The name of the user's profile.
    userProfileName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserContext' value with any optional fields omitted.
mkUserContext ::
  UserContext
mkUserContext =
  UserContext'
    { domainId = Core.Nothing,
      userProfileArn = Core.Nothing,
      userProfileName = Core.Nothing
    }

-- | The domain associated with the user.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDomainId :: Lens.Lens' UserContext (Core.Maybe Types.String)
ucDomainId = Lens.field @"domainId"
{-# DEPRECATED ucDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The Amazon Resource Name (ARN) of the user's profile.
--
-- /Note:/ Consider using 'userProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUserProfileArn :: Lens.Lens' UserContext (Core.Maybe Types.String)
ucUserProfileArn = Lens.field @"userProfileArn"
{-# DEPRECATED ucUserProfileArn "Use generic-lens or generic-optics with 'userProfileArn' instead." #-}

-- | The name of the user's profile.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUserProfileName :: Lens.Lens' UserContext (Core.Maybe Types.String)
ucUserProfileName = Lens.field @"userProfileName"
{-# DEPRECATED ucUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

instance Core.FromJSON UserContext where
  parseJSON =
    Core.withObject "UserContext" Core.$
      \x ->
        UserContext'
          Core.<$> (x Core..:? "DomainId")
          Core.<*> (x Core..:? "UserProfileArn")
          Core.<*> (x Core..:? "UserProfileName")
