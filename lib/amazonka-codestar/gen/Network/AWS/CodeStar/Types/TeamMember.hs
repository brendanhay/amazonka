-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.TeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.TeamMember
  ( TeamMember (..),

    -- * Smart constructor
    mkTeamMember,

    -- * Lenses
    tmRemoteAccessAllowed,
    tmUserARN,
    tmProjectRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a team member in a project.
--
-- /See:/ 'mkTeamMember' smart constructor.
data TeamMember = TeamMember'
  { remoteAccessAllowed ::
      Lude.Maybe Lude.Bool,
    userARN :: Lude.Text,
    projectRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TeamMember' with the minimum fields required to make a request.
--
-- * 'projectRole' - The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
-- * 'remoteAccessAllowed' - Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user in IAM.
mkTeamMember ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'projectRole'
  Lude.Text ->
  TeamMember
mkTeamMember pUserARN_ pProjectRole_ =
  TeamMember'
    { remoteAccessAllowed = Lude.Nothing,
      userARN = pUserARN_,
      projectRole = pProjectRole_
    }

-- | Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmRemoteAccessAllowed :: Lens.Lens' TeamMember (Lude.Maybe Lude.Bool)
tmRemoteAccessAllowed = Lens.lens (remoteAccessAllowed :: TeamMember -> Lude.Maybe Lude.Bool) (\s a -> s {remoteAccessAllowed = a} :: TeamMember)
{-# DEPRECATED tmRemoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead." #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmUserARN :: Lens.Lens' TeamMember Lude.Text
tmUserARN = Lens.lens (userARN :: TeamMember -> Lude.Text) (\s a -> s {userARN = a} :: TeamMember)
{-# DEPRECATED tmUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmProjectRole :: Lens.Lens' TeamMember Lude.Text
tmProjectRole = Lens.lens (projectRole :: TeamMember -> Lude.Text) (\s a -> s {projectRole = a} :: TeamMember)
{-# DEPRECATED tmProjectRole "Use generic-lens or generic-optics with 'projectRole' instead." #-}

instance Lude.FromJSON TeamMember where
  parseJSON =
    Lude.withObject
      "TeamMember"
      ( \x ->
          TeamMember'
            Lude.<$> (x Lude..:? "remoteAccessAllowed")
            Lude.<*> (x Lude..: "userArn")
            Lude.<*> (x Lude..: "projectRole")
      )
