{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.Types.RepositoryCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.RepositoryCredentials where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The repository credentials for private registry authentication.
--
-- /See:/ 'newRepositoryCredentials' smart constructor.
data RepositoryCredentials = RepositoryCredentials'
  { -- | The Amazon Resource Name (ARN) of the secret containing the private
    -- repository credentials.
    --
    -- When you are using the Amazon ECS API, AWS CLI, or AWS SDK, if the
    -- secret exists in the same Region as the task that you are launching then
    -- you can use either the full ARN or the name of the secret. When you are
    -- using the AWS Management Console, you must specify the full ARN of the
    -- secret.
    credentialsParameter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RepositoryCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsParameter', 'repositoryCredentials_credentialsParameter' - The Amazon Resource Name (ARN) of the secret containing the private
-- repository credentials.
--
-- When you are using the Amazon ECS API, AWS CLI, or AWS SDK, if the
-- secret exists in the same Region as the task that you are launching then
-- you can use either the full ARN or the name of the secret. When you are
-- using the AWS Management Console, you must specify the full ARN of the
-- secret.
newRepositoryCredentials ::
  -- | 'credentialsParameter'
  Prelude.Text ->
  RepositoryCredentials
newRepositoryCredentials pCredentialsParameter_ =
  RepositoryCredentials'
    { credentialsParameter =
        pCredentialsParameter_
    }

-- | The Amazon Resource Name (ARN) of the secret containing the private
-- repository credentials.
--
-- When you are using the Amazon ECS API, AWS CLI, or AWS SDK, if the
-- secret exists in the same Region as the task that you are launching then
-- you can use either the full ARN or the name of the secret. When you are
-- using the AWS Management Console, you must specify the full ARN of the
-- secret.
repositoryCredentials_credentialsParameter :: Lens.Lens' RepositoryCredentials Prelude.Text
repositoryCredentials_credentialsParameter = Lens.lens (\RepositoryCredentials' {credentialsParameter} -> credentialsParameter) (\s@RepositoryCredentials' {} a -> s {credentialsParameter = a} :: RepositoryCredentials)

instance Prelude.FromJSON RepositoryCredentials where
  parseJSON =
    Prelude.withObject
      "RepositoryCredentials"
      ( \x ->
          RepositoryCredentials'
            Prelude.<$> (x Prelude..: "credentialsParameter")
      )

instance Prelude.Hashable RepositoryCredentials

instance Prelude.NFData RepositoryCredentials

instance Prelude.ToJSON RepositoryCredentials where
  toJSON RepositoryCredentials' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "credentialsParameter"
                  Prelude..= credentialsParameter
              )
          ]
      )
