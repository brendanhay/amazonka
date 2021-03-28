{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.TeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.TeamMember
  ( TeamMember (..)
  -- * Smart constructor
  , mkTeamMember
  -- * Lenses
  , tmUserArn
  , tmProjectRole
  , tmRemoteAccessAllowed
  ) where

import qualified Network.AWS.CodeStar.Types.ProjectRole as Types
import qualified Network.AWS.CodeStar.Types.UserArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a team member in a project.
--
-- /See:/ 'mkTeamMember' smart constructor.
data TeamMember = TeamMember'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user in IAM.
  , projectRole :: Types.ProjectRole
    -- ^ The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ . 
  , remoteAccessAllowed :: Core.Maybe Core.Bool
    -- ^ Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TeamMember' value with any optional fields omitted.
mkTeamMember
    :: Types.UserArn -- ^ 'userArn'
    -> Types.ProjectRole -- ^ 'projectRole'
    -> TeamMember
mkTeamMember userArn projectRole
  = TeamMember'{userArn, projectRole,
                remoteAccessAllowed = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmUserArn :: Lens.Lens' TeamMember Types.UserArn
tmUserArn = Lens.field @"userArn"
{-# INLINEABLE tmUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ . 
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmProjectRole :: Lens.Lens' TeamMember Types.ProjectRole
tmProjectRole = Lens.field @"projectRole"
{-# INLINEABLE tmProjectRole #-}
{-# DEPRECATED projectRole "Use generic-lens or generic-optics with 'projectRole' instead"  #-}

-- | Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmRemoteAccessAllowed :: Lens.Lens' TeamMember (Core.Maybe Core.Bool)
tmRemoteAccessAllowed = Lens.field @"remoteAccessAllowed"
{-# INLINEABLE tmRemoteAccessAllowed #-}
{-# DEPRECATED remoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead"  #-}

instance Core.FromJSON TeamMember where
        parseJSON
          = Core.withObject "TeamMember" Core.$
              \ x ->
                TeamMember' Core.<$>
                  (x Core..: "userArn") Core.<*> x Core..: "projectRole" Core.<*>
                    x Core..:? "remoteAccessAllowed"
