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
-- Module      : Network.AWS.SageMaker.Types.GitConfigForUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfigForUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies configuration details for a Git repository when the repository
-- is updated.
--
-- /See:/ 'newGitConfigForUpdate' smart constructor.
data GitConfigForUpdate = GitConfigForUpdate'
  { -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
    -- contains the credentials used to access the git repository. The secret
    -- must have a staging label of @AWSCURRENT@ and must be in the following
    -- format:
    --
    -- @{\"username\": UserName, \"password\": Password}@
    secretArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GitConfigForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretArn', 'gitConfigForUpdate_secretArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the git repository. The secret
-- must have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
newGitConfigForUpdate ::
  GitConfigForUpdate
newGitConfigForUpdate =
  GitConfigForUpdate' {secretArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the git repository. The secret
-- must have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
gitConfigForUpdate_secretArn :: Lens.Lens' GitConfigForUpdate (Core.Maybe Core.Text)
gitConfigForUpdate_secretArn = Lens.lens (\GitConfigForUpdate' {secretArn} -> secretArn) (\s@GitConfigForUpdate' {} a -> s {secretArn = a} :: GitConfigForUpdate)

instance Core.Hashable GitConfigForUpdate

instance Core.NFData GitConfigForUpdate

instance Core.ToJSON GitConfigForUpdate where
  toJSON GitConfigForUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [("SecretArn" Core..=) Core.<$> secretArn]
      )
