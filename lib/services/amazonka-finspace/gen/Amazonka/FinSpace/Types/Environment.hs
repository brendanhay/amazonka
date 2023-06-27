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
-- Module      : Amazonka.FinSpace.Types.Environment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.Environment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.EnvironmentStatus
import Amazonka.FinSpace.Types.FederationMode
import Amazonka.FinSpace.Types.FederationParameters
import qualified Amazonka.Prelude as Prelude

-- | Represents an FinSpace environment.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The ID of the AWS account in which the FinSpace environment is created.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the dedicated service account associated with your
    -- FinSpace environment.
    dedicatedServiceAccountId :: Prelude.Maybe Prelude.Text,
    -- | The description of the FinSpace environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of your FinSpace environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the FinSpace environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The sign-in URL for the web application of your FinSpace environment.
    environmentUrl :: Prelude.Maybe Prelude.Text,
    -- | The authentication mode for the environment.
    federationMode :: Prelude.Maybe FederationMode,
    -- | Configuration information when authentication mode is FEDERATED.
    federationParameters :: Prelude.Maybe FederationParameters,
    -- | The KMS key id used to encrypt in the FinSpace environment.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the FinSpace environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL of the integrated FinSpace notebook environment in your web
    -- application.
    sageMakerStudioDomainUrl :: Prelude.Maybe Prelude.Text,
    -- | The current status of creation of the FinSpace environment.
    status :: Prelude.Maybe EnvironmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'environment_awsAccountId' - The ID of the AWS account in which the FinSpace environment is created.
--
-- 'dedicatedServiceAccountId', 'environment_dedicatedServiceAccountId' - The AWS account ID of the dedicated service account associated with your
-- FinSpace environment.
--
-- 'description', 'environment_description' - The description of the FinSpace environment.
--
-- 'environmentArn', 'environment_environmentArn' - The Amazon Resource Name (ARN) of your FinSpace environment.
--
-- 'environmentId', 'environment_environmentId' - The identifier of the FinSpace environment.
--
-- 'environmentUrl', 'environment_environmentUrl' - The sign-in URL for the web application of your FinSpace environment.
--
-- 'federationMode', 'environment_federationMode' - The authentication mode for the environment.
--
-- 'federationParameters', 'environment_federationParameters' - Configuration information when authentication mode is FEDERATED.
--
-- 'kmsKeyId', 'environment_kmsKeyId' - The KMS key id used to encrypt in the FinSpace environment.
--
-- 'name', 'environment_name' - The name of the FinSpace environment.
--
-- 'sageMakerStudioDomainUrl', 'environment_sageMakerStudioDomainUrl' - The URL of the integrated FinSpace notebook environment in your web
-- application.
--
-- 'status', 'environment_status' - The current status of creation of the FinSpace environment.
newEnvironment ::
  Environment
newEnvironment =
  Environment'
    { awsAccountId = Prelude.Nothing,
      dedicatedServiceAccountId = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentUrl = Prelude.Nothing,
      federationMode = Prelude.Nothing,
      federationParameters = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      sageMakerStudioDomainUrl = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of the AWS account in which the FinSpace environment is created.
environment_awsAccountId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_awsAccountId = Lens.lens (\Environment' {awsAccountId} -> awsAccountId) (\s@Environment' {} a -> s {awsAccountId = a} :: Environment)

-- | The AWS account ID of the dedicated service account associated with your
-- FinSpace environment.
environment_dedicatedServiceAccountId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_dedicatedServiceAccountId = Lens.lens (\Environment' {dedicatedServiceAccountId} -> dedicatedServiceAccountId) (\s@Environment' {} a -> s {dedicatedServiceAccountId = a} :: Environment)

-- | The description of the FinSpace environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment)

-- | The Amazon Resource Name (ARN) of your FinSpace environment.
environment_environmentArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentArn = Lens.lens (\Environment' {environmentArn} -> environmentArn) (\s@Environment' {} a -> s {environmentArn = a} :: Environment)

-- | The identifier of the FinSpace environment.
environment_environmentId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentId = Lens.lens (\Environment' {environmentId} -> environmentId) (\s@Environment' {} a -> s {environmentId = a} :: Environment)

-- | The sign-in URL for the web application of your FinSpace environment.
environment_environmentUrl :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentUrl = Lens.lens (\Environment' {environmentUrl} -> environmentUrl) (\s@Environment' {} a -> s {environmentUrl = a} :: Environment)

-- | The authentication mode for the environment.
environment_federationMode :: Lens.Lens' Environment (Prelude.Maybe FederationMode)
environment_federationMode = Lens.lens (\Environment' {federationMode} -> federationMode) (\s@Environment' {} a -> s {federationMode = a} :: Environment)

-- | Configuration information when authentication mode is FEDERATED.
environment_federationParameters :: Lens.Lens' Environment (Prelude.Maybe FederationParameters)
environment_federationParameters = Lens.lens (\Environment' {federationParameters} -> federationParameters) (\s@Environment' {} a -> s {federationParameters = a} :: Environment)

-- | The KMS key id used to encrypt in the FinSpace environment.
environment_kmsKeyId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_kmsKeyId = Lens.lens (\Environment' {kmsKeyId} -> kmsKeyId) (\s@Environment' {} a -> s {kmsKeyId = a} :: Environment)

-- | The name of the FinSpace environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The URL of the integrated FinSpace notebook environment in your web
-- application.
environment_sageMakerStudioDomainUrl :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_sageMakerStudioDomainUrl = Lens.lens (\Environment' {sageMakerStudioDomainUrl} -> sageMakerStudioDomainUrl) (\s@Environment' {} a -> s {sageMakerStudioDomainUrl = a} :: Environment)

-- | The current status of creation of the FinSpace environment.
environment_status :: Lens.Lens' Environment (Prelude.Maybe EnvironmentStatus)
environment_status = Lens.lens (\Environment' {status} -> status) (\s@Environment' {} a -> s {status = a} :: Environment)

instance Data.FromJSON Environment where
  parseJSON =
    Data.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "dedicatedServiceAccountId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "environmentArn")
            Prelude.<*> (x Data..:? "environmentId")
            Prelude.<*> (x Data..:? "environmentUrl")
            Prelude.<*> (x Data..:? "federationMode")
            Prelude.<*> (x Data..:? "federationParameters")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sageMakerStudioDomainUrl")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Environment where
  hashWithSalt _salt Environment' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dedicatedServiceAccountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentArn
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentUrl
      `Prelude.hashWithSalt` federationMode
      `Prelude.hashWithSalt` federationParameters
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sageMakerStudioDomainUrl
      `Prelude.hashWithSalt` status

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dedicatedServiceAccountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentUrl
      `Prelude.seq` Prelude.rnf federationMode
      `Prelude.seq` Prelude.rnf federationParameters
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sageMakerStudioDomainUrl
      `Prelude.seq` Prelude.rnf status
