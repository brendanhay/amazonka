{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.UserProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.UserProfileSummary
  ( UserProfileSummary (..)
  -- * Smart constructor
  , mkUserProfileSummary
  -- * Lenses
  , upsDisplayName
  , upsEmailAddress
  , upsSshPublicKey
  , upsUserArn
  ) where

import qualified Network.AWS.CodeStar.Types.Email as Types
import qualified Network.AWS.CodeStar.Types.SshPublicKey as Types
import qualified Network.AWS.CodeStar.Types.UserArn as Types
import qualified Network.AWS.CodeStar.Types.UserProfileDisplayName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a user's profile in AWS CodeStar.
--
-- /See:/ 'mkUserProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { displayName :: Core.Maybe Types.UserProfileDisplayName
    -- ^ The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
  , emailAddress :: Core.Maybe Types.Email
    -- ^ The email address associated with the user.
  , sshPublicKey :: Core.Maybe Types.SshPublicKey
    -- ^ The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
  , userArn :: Core.Maybe Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user in IAM.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserProfileSummary' value with any optional fields omitted.
mkUserProfileSummary
    :: UserProfileSummary
mkUserProfileSummary
  = UserProfileSummary'{displayName = Core.Nothing,
                        emailAddress = Core.Nothing, sshPublicKey = Core.Nothing,
                        userArn = Core.Nothing}

-- | The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDisplayName :: Lens.Lens' UserProfileSummary (Core.Maybe Types.UserProfileDisplayName)
upsDisplayName = Lens.field @"displayName"
{-# INLINEABLE upsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email address associated with the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsEmailAddress :: Lens.Lens' UserProfileSummary (Core.Maybe Types.Email)
upsEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE upsEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsSshPublicKey :: Lens.Lens' UserProfileSummary (Core.Maybe Types.SshPublicKey)
upsSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE upsSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsUserArn :: Lens.Lens' UserProfileSummary (Core.Maybe Types.UserArn)
upsUserArn = Lens.field @"userArn"
{-# INLINEABLE upsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.FromJSON UserProfileSummary where
        parseJSON
          = Core.withObject "UserProfileSummary" Core.$
              \ x ->
                UserProfileSummary' Core.<$>
                  (x Core..:? "displayName") Core.<*> x Core..:? "emailAddress"
                    Core.<*> x Core..:? "sshPublicKey"
                    Core.<*> x Core..:? "userArn"
