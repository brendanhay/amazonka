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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRoleRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRoleRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a request to activate or deactivate the role that you can use
-- to grant an Amazon Lightsail container service access to Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
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
-- /See:/ 'newContainerServiceECRImagePullerRoleRequest' smart constructor.
data ContainerServiceECRImagePullerRoleRequest = ContainerServiceECRImagePullerRoleRequest'
  { -- | A Boolean value that indicates whether to activate the role.
    isActive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceECRImagePullerRoleRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isActive', 'containerServiceECRImagePullerRoleRequest_isActive' - A Boolean value that indicates whether to activate the role.
newContainerServiceECRImagePullerRoleRequest ::
  ContainerServiceECRImagePullerRoleRequest
newContainerServiceECRImagePullerRoleRequest =
  ContainerServiceECRImagePullerRoleRequest'
    { isActive =
        Prelude.Nothing
    }

-- | A Boolean value that indicates whether to activate the role.
containerServiceECRImagePullerRoleRequest_isActive :: Lens.Lens' ContainerServiceECRImagePullerRoleRequest (Prelude.Maybe Prelude.Bool)
containerServiceECRImagePullerRoleRequest_isActive = Lens.lens (\ContainerServiceECRImagePullerRoleRequest' {isActive} -> isActive) (\s@ContainerServiceECRImagePullerRoleRequest' {} a -> s {isActive = a} :: ContainerServiceECRImagePullerRoleRequest)

instance
  Prelude.Hashable
    ContainerServiceECRImagePullerRoleRequest
  where
  hashWithSalt
    _salt
    ContainerServiceECRImagePullerRoleRequest' {..} =
      _salt `Prelude.hashWithSalt` isActive

instance
  Prelude.NFData
    ContainerServiceECRImagePullerRoleRequest
  where
  rnf ContainerServiceECRImagePullerRoleRequest' {..} =
    Prelude.rnf isActive

instance
  Data.ToJSON
    ContainerServiceECRImagePullerRoleRequest
  where
  toJSON ContainerServiceECRImagePullerRoleRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("isActive" Data..=) Prelude.<$> isActive]
      )
