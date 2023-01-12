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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the activation status of the role that you can use to grant an
-- Amazon Lightsail container service access to Amazon Elastic Container
-- Registry (Amazon ECR) private repositories.
--
-- When activated, Lightsail creates an Identity and Access Management
-- (IAM) role for the specified Lightsail container service. You can use
-- the ARN of the role to create a trust relationship between your
-- Lightsail container service and an Amazon ECR private repository in your
-- Amazon Web Services account. This allows your container service to pull
-- images from Amazon ECR private repositories. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newContainerServiceECRImagePullerRole' smart constructor.
data ContainerServiceECRImagePullerRole = ContainerServiceECRImagePullerRole'
  { -- | A Boolean value that indicates whether the role is activated.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the role, if it is activated.
    principalArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceECRImagePullerRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isActive', 'containerServiceECRImagePullerRole_isActive' - A Boolean value that indicates whether the role is activated.
--
-- 'principalArn', 'containerServiceECRImagePullerRole_principalArn' - The Amazon Resource Name (ARN) of the role, if it is activated.
newContainerServiceECRImagePullerRole ::
  ContainerServiceECRImagePullerRole
newContainerServiceECRImagePullerRole =
  ContainerServiceECRImagePullerRole'
    { isActive =
        Prelude.Nothing,
      principalArn = Prelude.Nothing
    }

-- | A Boolean value that indicates whether the role is activated.
containerServiceECRImagePullerRole_isActive :: Lens.Lens' ContainerServiceECRImagePullerRole (Prelude.Maybe Prelude.Bool)
containerServiceECRImagePullerRole_isActive = Lens.lens (\ContainerServiceECRImagePullerRole' {isActive} -> isActive) (\s@ContainerServiceECRImagePullerRole' {} a -> s {isActive = a} :: ContainerServiceECRImagePullerRole)

-- | The Amazon Resource Name (ARN) of the role, if it is activated.
containerServiceECRImagePullerRole_principalArn :: Lens.Lens' ContainerServiceECRImagePullerRole (Prelude.Maybe Prelude.Text)
containerServiceECRImagePullerRole_principalArn = Lens.lens (\ContainerServiceECRImagePullerRole' {principalArn} -> principalArn) (\s@ContainerServiceECRImagePullerRole' {} a -> s {principalArn = a} :: ContainerServiceECRImagePullerRole)

instance
  Data.FromJSON
    ContainerServiceECRImagePullerRole
  where
  parseJSON =
    Data.withObject
      "ContainerServiceECRImagePullerRole"
      ( \x ->
          ContainerServiceECRImagePullerRole'
            Prelude.<$> (x Data..:? "isActive")
            Prelude.<*> (x Data..:? "principalArn")
      )

instance
  Prelude.Hashable
    ContainerServiceECRImagePullerRole
  where
  hashWithSalt
    _salt
    ContainerServiceECRImagePullerRole' {..} =
      _salt `Prelude.hashWithSalt` isActive
        `Prelude.hashWithSalt` principalArn

instance
  Prelude.NFData
    ContainerServiceECRImagePullerRole
  where
  rnf ContainerServiceECRImagePullerRole' {..} =
    Prelude.rnf isActive
      `Prelude.seq` Prelude.rnf principalArn
