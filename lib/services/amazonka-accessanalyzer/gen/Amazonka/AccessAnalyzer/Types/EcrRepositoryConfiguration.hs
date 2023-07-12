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
-- Module      : Amazonka.AccessAnalyzer.Types.EcrRepositoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.EcrRepositoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an Amazon ECR repository.
-- You can propose a configuration for a new Amazon ECR repository or an
-- existing Amazon ECR repository that you own by specifying the Amazon ECR
-- policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/APIReference/API_Repository.html Repository>.
--
-- -   If the configuration is for an existing Amazon ECR repository and
--     you do not specify the Amazon ECR policy, then the access preview
--     uses the existing Amazon ECR policy for the repository.
--
-- -   If the access preview is for a new resource and you do not specify
--     the policy, then the access preview assumes an Amazon ECR repository
--     without a policy.
--
-- -   To propose deletion of an existing Amazon ECR repository policy, you
--     can specify an empty string for the Amazon ECR policy.
--
-- /See:/ 'newEcrRepositoryConfiguration' smart constructor.
data EcrRepositoryConfiguration = EcrRepositoryConfiguration'
  { -- | The JSON repository policy text to apply to the Amazon ECR repository.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Private repository policy examples>
    -- in the /Amazon ECR User Guide/.
    repositoryPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrRepositoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryPolicy', 'ecrRepositoryConfiguration_repositoryPolicy' - The JSON repository policy text to apply to the Amazon ECR repository.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Private repository policy examples>
-- in the /Amazon ECR User Guide/.
newEcrRepositoryConfiguration ::
  EcrRepositoryConfiguration
newEcrRepositoryConfiguration =
  EcrRepositoryConfiguration'
    { repositoryPolicy =
        Prelude.Nothing
    }

-- | The JSON repository policy text to apply to the Amazon ECR repository.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Private repository policy examples>
-- in the /Amazon ECR User Guide/.
ecrRepositoryConfiguration_repositoryPolicy :: Lens.Lens' EcrRepositoryConfiguration (Prelude.Maybe Prelude.Text)
ecrRepositoryConfiguration_repositoryPolicy = Lens.lens (\EcrRepositoryConfiguration' {repositoryPolicy} -> repositoryPolicy) (\s@EcrRepositoryConfiguration' {} a -> s {repositoryPolicy = a} :: EcrRepositoryConfiguration)

instance Data.FromJSON EcrRepositoryConfiguration where
  parseJSON =
    Data.withObject
      "EcrRepositoryConfiguration"
      ( \x ->
          EcrRepositoryConfiguration'
            Prelude.<$> (x Data..:? "repositoryPolicy")
      )

instance Prelude.Hashable EcrRepositoryConfiguration where
  hashWithSalt _salt EcrRepositoryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` repositoryPolicy

instance Prelude.NFData EcrRepositoryConfiguration where
  rnf EcrRepositoryConfiguration' {..} =
    Prelude.rnf repositoryPolicy

instance Data.ToJSON EcrRepositoryConfiguration where
  toJSON EcrRepositoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("repositoryPolicy" Data..=)
              Prelude.<$> repositoryPolicy
          ]
      )
