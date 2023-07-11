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
-- Module      : Amazonka.SageMaker.Types.RepositoryAuthConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RepositoryAuthConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an authentication configuration for the private docker
-- registry where your model image is hosted. Specify a value for this
-- property only if you specified @Vpc@ as the value for the
-- @RepositoryAccessMode@ field of the @ImageConfig@ object that you passed
-- to a call to @CreateModel@ and the private Docker registry where the
-- model image is hosted requires authentication.
--
-- /See:/ 'newRepositoryAuthConfig' smart constructor.
data RepositoryAuthConfig = RepositoryAuthConfig'
  { -- | The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
    -- that provides credentials to authenticate to the private Docker registry
    -- where your model image is hosted. For information about how to create an
    -- Amazon Web Services Lambda function, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/getting-started-create-function.html Create a Lambda function with the console>
    -- in the /Amazon Web Services Lambda Developer Guide/.
    repositoryCredentialsProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryCredentialsProviderArn', 'repositoryAuthConfig_repositoryCredentialsProviderArn' - The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
-- that provides credentials to authenticate to the private Docker registry
-- where your model image is hosted. For information about how to create an
-- Amazon Web Services Lambda function, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/getting-started-create-function.html Create a Lambda function with the console>
-- in the /Amazon Web Services Lambda Developer Guide/.
newRepositoryAuthConfig ::
  -- | 'repositoryCredentialsProviderArn'
  Prelude.Text ->
  RepositoryAuthConfig
newRepositoryAuthConfig
  pRepositoryCredentialsProviderArn_ =
    RepositoryAuthConfig'
      { repositoryCredentialsProviderArn =
          pRepositoryCredentialsProviderArn_
      }

-- | The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
-- that provides credentials to authenticate to the private Docker registry
-- where your model image is hosted. For information about how to create an
-- Amazon Web Services Lambda function, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/getting-started-create-function.html Create a Lambda function with the console>
-- in the /Amazon Web Services Lambda Developer Guide/.
repositoryAuthConfig_repositoryCredentialsProviderArn :: Lens.Lens' RepositoryAuthConfig Prelude.Text
repositoryAuthConfig_repositoryCredentialsProviderArn = Lens.lens (\RepositoryAuthConfig' {repositoryCredentialsProviderArn} -> repositoryCredentialsProviderArn) (\s@RepositoryAuthConfig' {} a -> s {repositoryCredentialsProviderArn = a} :: RepositoryAuthConfig)

instance Data.FromJSON RepositoryAuthConfig where
  parseJSON =
    Data.withObject
      "RepositoryAuthConfig"
      ( \x ->
          RepositoryAuthConfig'
            Prelude.<$> (x Data..: "RepositoryCredentialsProviderArn")
      )

instance Prelude.Hashable RepositoryAuthConfig where
  hashWithSalt _salt RepositoryAuthConfig' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryCredentialsProviderArn

instance Prelude.NFData RepositoryAuthConfig where
  rnf RepositoryAuthConfig' {..} =
    Prelude.rnf repositoryCredentialsProviderArn

instance Data.ToJSON RepositoryAuthConfig where
  toJSON RepositoryAuthConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RepositoryCredentialsProviderArn"
                  Data..= repositoryCredentialsProviderArn
              )
          ]
      )
