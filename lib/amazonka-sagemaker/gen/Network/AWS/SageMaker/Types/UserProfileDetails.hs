{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileDetails
  ( UserProfileDetails (..),

    -- * Smart constructor
    mkUserProfileDetails,

    -- * Lenses
    updCreationTime,
    updDomainId,
    updLastModifiedTime,
    updStatus,
    updUserProfileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DomainId as Types
import qualified Network.AWS.SageMaker.Types.UserProfileName as Types
import qualified Network.AWS.SageMaker.Types.UserProfileStatus as Types

-- | The user profile details.
--
-- /See:/ 'mkUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { -- | The creation time.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The domain ID.
    domainId :: Core.Maybe Types.DomainId,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status.
    status :: Core.Maybe Types.UserProfileStatus,
    -- | The user profile name.
    userProfileName :: Core.Maybe Types.UserProfileName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UserProfileDetails' value with any optional fields omitted.
mkUserProfileDetails ::
  UserProfileDetails
mkUserProfileDetails =
  UserProfileDetails'
    { creationTime = Core.Nothing,
      domainId = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      status = Core.Nothing,
      userProfileName = Core.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updCreationTime :: Lens.Lens' UserProfileDetails (Core.Maybe Core.NominalDiffTime)
updCreationTime = Lens.field @"creationTime"
{-# DEPRECATED updCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDomainId :: Lens.Lens' UserProfileDetails (Core.Maybe Types.DomainId)
updDomainId = Lens.field @"domainId"
{-# DEPRECATED updDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updLastModifiedTime :: Lens.Lens' UserProfileDetails (Core.Maybe Core.NominalDiffTime)
updLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED updLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updStatus :: Lens.Lens' UserProfileDetails (Core.Maybe Types.UserProfileStatus)
updStatus = Lens.field @"status"
{-# DEPRECATED updStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updUserProfileName :: Lens.Lens' UserProfileDetails (Core.Maybe Types.UserProfileName)
updUserProfileName = Lens.field @"userProfileName"
{-# DEPRECATED updUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

instance Core.FromJSON UserProfileDetails where
  parseJSON =
    Core.withObject "UserProfileDetails" Core.$
      \x ->
        UserProfileDetails'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DomainId")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UserProfileName")
