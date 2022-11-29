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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The private repository authentication credentials to use.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails = AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails'
  { -- | The ARN of the secret that contains the private repository credentials.
    credentialsParameter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsParameter', 'awsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails_credentialsParameter' - The ARN of the secret that contains the private repository credentials.
newAwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
newAwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails'
    { credentialsParameter =
        Prelude.Nothing
    }

-- | The ARN of the secret that contains the private repository credentials.
awsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails_credentialsParameter :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails_credentialsParameter = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' {credentialsParameter} -> credentialsParameter) (\s@AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' {} a -> s {credentialsParameter = a} :: AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails'
            Prelude.<$> (x Core..:? "CredentialsParameter")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' {..} =
      _salt `Prelude.hashWithSalt` credentialsParameter

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' {..} =
      Prelude.rnf credentialsParameter

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("CredentialsParameter" Core..=)
                Prelude.<$> credentialsParameter
            ]
        )
