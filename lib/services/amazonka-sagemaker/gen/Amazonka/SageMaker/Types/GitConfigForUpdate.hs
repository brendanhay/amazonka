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
-- Module      : Amazonka.SageMaker.Types.GitConfigForUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.GitConfigForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration details for a Git repository when the repository
-- is updated.
--
-- /See:/ 'newGitConfigForUpdate' smart constructor.
data GitConfigForUpdate = GitConfigForUpdate'
  { -- | The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
    -- Manager secret that contains the credentials used to access the git
    -- repository. The secret must have a staging label of @AWSCURRENT@ and
    -- must be in the following format:
    --
    -- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
    secretArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GitConfigForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretArn', 'gitConfigForUpdate_secretArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
-- Manager secret that contains the credentials used to access the git
-- repository. The secret must have a staging label of @AWSCURRENT@ and
-- must be in the following format:
--
-- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
newGitConfigForUpdate ::
  GitConfigForUpdate
newGitConfigForUpdate =
  GitConfigForUpdate' {secretArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
-- Manager secret that contains the credentials used to access the git
-- repository. The secret must have a staging label of @AWSCURRENT@ and
-- must be in the following format:
--
-- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
gitConfigForUpdate_secretArn :: Lens.Lens' GitConfigForUpdate (Prelude.Maybe Prelude.Text)
gitConfigForUpdate_secretArn = Lens.lens (\GitConfigForUpdate' {secretArn} -> secretArn) (\s@GitConfigForUpdate' {} a -> s {secretArn = a} :: GitConfigForUpdate)

instance Prelude.Hashable GitConfigForUpdate where
  hashWithSalt _salt GitConfigForUpdate' {..} =
    _salt `Prelude.hashWithSalt` secretArn

instance Prelude.NFData GitConfigForUpdate where
  rnf GitConfigForUpdate' {..} = Prelude.rnf secretArn

instance Data.ToJSON GitConfigForUpdate where
  toJSON GitConfigForUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SecretArn" Data..=) Prelude.<$> secretArn]
      )
