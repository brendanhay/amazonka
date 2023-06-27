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
-- Module      : Amazonka.SageMaker.Types.TrainingRepositoryAuthConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingRepositoryAuthConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object containing authentication information for a private Docker
-- registry.
--
-- /See:/ 'newTrainingRepositoryAuthConfig' smart constructor.
data TrainingRepositoryAuthConfig = TrainingRepositoryAuthConfig'
  { -- | The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
    -- used to give SageMaker access credentials to your private Docker
    -- registry.
    trainingRepositoryCredentialsProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingRepositoryAuthConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingRepositoryCredentialsProviderArn', 'trainingRepositoryAuthConfig_trainingRepositoryCredentialsProviderArn' - The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
-- used to give SageMaker access credentials to your private Docker
-- registry.
newTrainingRepositoryAuthConfig ::
  -- | 'trainingRepositoryCredentialsProviderArn'
  Prelude.Text ->
  TrainingRepositoryAuthConfig
newTrainingRepositoryAuthConfig
  pTrainingRepositoryCredentialsProviderArn_ =
    TrainingRepositoryAuthConfig'
      { trainingRepositoryCredentialsProviderArn =
          pTrainingRepositoryCredentialsProviderArn_
      }

-- | The Amazon Resource Name (ARN) of an Amazon Web Services Lambda function
-- used to give SageMaker access credentials to your private Docker
-- registry.
trainingRepositoryAuthConfig_trainingRepositoryCredentialsProviderArn :: Lens.Lens' TrainingRepositoryAuthConfig Prelude.Text
trainingRepositoryAuthConfig_trainingRepositoryCredentialsProviderArn = Lens.lens (\TrainingRepositoryAuthConfig' {trainingRepositoryCredentialsProviderArn} -> trainingRepositoryCredentialsProviderArn) (\s@TrainingRepositoryAuthConfig' {} a -> s {trainingRepositoryCredentialsProviderArn = a} :: TrainingRepositoryAuthConfig)

instance Data.FromJSON TrainingRepositoryAuthConfig where
  parseJSON =
    Data.withObject
      "TrainingRepositoryAuthConfig"
      ( \x ->
          TrainingRepositoryAuthConfig'
            Prelude.<$> ( x
                            Data..: "TrainingRepositoryCredentialsProviderArn"
                        )
      )

instance
  Prelude.Hashable
    TrainingRepositoryAuthConfig
  where
  hashWithSalt _salt TrainingRepositoryAuthConfig' {..} =
    _salt
      `Prelude.hashWithSalt` trainingRepositoryCredentialsProviderArn

instance Prelude.NFData TrainingRepositoryAuthConfig where
  rnf TrainingRepositoryAuthConfig' {..} =
    Prelude.rnf
      trainingRepositoryCredentialsProviderArn

instance Data.ToJSON TrainingRepositoryAuthConfig where
  toJSON TrainingRepositoryAuthConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TrainingRepositoryCredentialsProviderArn"
                  Data..= trainingRepositoryCredentialsProviderArn
              )
          ]
      )
