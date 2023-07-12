{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStar.Types.TeamMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.TeamMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a team member in a project.
--
-- /See:/ 'newTeamMember' smart constructor.
data TeamMember = TeamMember'
  { -- | Whether the user is allowed to remotely access project resources using
    -- an SSH public\/private key pair.
    remoteAccessAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Prelude.Text,
    -- | The role assigned to the user in the project. Project roles have
    -- different levels of access. For more information, see
    -- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
    -- in the /AWS CodeStar User Guide/.
    projectRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TeamMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessAllowed', 'teamMember_remoteAccessAllowed' - Whether the user is allowed to remotely access project resources using
-- an SSH public\/private key pair.
--
-- 'userArn', 'teamMember_userArn' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- 'projectRole', 'teamMember_projectRole' - The role assigned to the user in the project. Project roles have
-- different levels of access. For more information, see
-- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
-- in the /AWS CodeStar User Guide/.
newTeamMember ::
  -- | 'userArn'
  Prelude.Text ->
  -- | 'projectRole'
  Prelude.Text ->
  TeamMember
newTeamMember pUserArn_ pProjectRole_ =
  TeamMember'
    { remoteAccessAllowed = Prelude.Nothing,
      userArn = pUserArn_,
      projectRole = pProjectRole_
    }

-- | Whether the user is allowed to remotely access project resources using
-- an SSH public\/private key pair.
teamMember_remoteAccessAllowed :: Lens.Lens' TeamMember (Prelude.Maybe Prelude.Bool)
teamMember_remoteAccessAllowed = Lens.lens (\TeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@TeamMember' {} a -> s {remoteAccessAllowed = a} :: TeamMember)

-- | The Amazon Resource Name (ARN) of the user in IAM.
teamMember_userArn :: Lens.Lens' TeamMember Prelude.Text
teamMember_userArn = Lens.lens (\TeamMember' {userArn} -> userArn) (\s@TeamMember' {} a -> s {userArn = a} :: TeamMember)

-- | The role assigned to the user in the project. Project roles have
-- different levels of access. For more information, see
-- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
-- in the /AWS CodeStar User Guide/.
teamMember_projectRole :: Lens.Lens' TeamMember Prelude.Text
teamMember_projectRole = Lens.lens (\TeamMember' {projectRole} -> projectRole) (\s@TeamMember' {} a -> s {projectRole = a} :: TeamMember)

instance Data.FromJSON TeamMember where
  parseJSON =
    Data.withObject
      "TeamMember"
      ( \x ->
          TeamMember'
            Prelude.<$> (x Data..:? "remoteAccessAllowed")
            Prelude.<*> (x Data..: "userArn")
            Prelude.<*> (x Data..: "projectRole")
      )

instance Prelude.Hashable TeamMember where
  hashWithSalt _salt TeamMember' {..} =
    _salt
      `Prelude.hashWithSalt` remoteAccessAllowed
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` projectRole

instance Prelude.NFData TeamMember where
  rnf TeamMember' {..} =
    Prelude.rnf remoteAccessAllowed
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf projectRole
