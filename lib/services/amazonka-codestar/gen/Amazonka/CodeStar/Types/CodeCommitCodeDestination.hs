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
-- Module      : Amazonka.CodeStar.Types.CodeCommitCodeDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.CodeCommitCodeDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the AWS CodeCommit repository to be created in AWS
-- CodeStar. This is where the source code files provided with the project
-- request will be uploaded after project creation.
--
-- /See:/ 'newCodeCommitCodeDestination' smart constructor.
data CodeCommitCodeDestination = CodeCommitCodeDestination'
  { -- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeCommitCodeDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'codeCommitCodeDestination_name' - The name of the AWS CodeCommit repository to be created in AWS CodeStar.
newCodeCommitCodeDestination ::
  -- | 'name'
  Prelude.Text ->
  CodeCommitCodeDestination
newCodeCommitCodeDestination pName_ =
  CodeCommitCodeDestination' {name = pName_}

-- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
codeCommitCodeDestination_name :: Lens.Lens' CodeCommitCodeDestination Prelude.Text
codeCommitCodeDestination_name = Lens.lens (\CodeCommitCodeDestination' {name} -> name) (\s@CodeCommitCodeDestination' {} a -> s {name = a} :: CodeCommitCodeDestination)

instance Prelude.Hashable CodeCommitCodeDestination where
  hashWithSalt _salt CodeCommitCodeDestination' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CodeCommitCodeDestination where
  rnf CodeCommitCodeDestination' {..} = Prelude.rnf name

instance Core.ToJSON CodeCommitCodeDestination where
  toJSON CodeCommitCodeDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )
