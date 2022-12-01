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
-- Module      : Amazonka.Lightsail.Types.PrivateRegistryAccessRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PrivateRegistryAccessRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRoleRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a request to configure an Amazon Lightsail container service
-- to access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newPrivateRegistryAccessRequest' smart constructor.
data PrivateRegistryAccessRequest = PrivateRegistryAccessRequest'
  { -- | An object to describe a request to activate or deactivate the role that
    -- you can use to grant an Amazon Lightsail container service access to
    -- Amazon Elastic Container Registry (Amazon ECR) private repositories.
    ecrImagePullerRole :: Prelude.Maybe ContainerServiceECRImagePullerRoleRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateRegistryAccessRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecrImagePullerRole', 'privateRegistryAccessRequest_ecrImagePullerRole' - An object to describe a request to activate or deactivate the role that
-- you can use to grant an Amazon Lightsail container service access to
-- Amazon Elastic Container Registry (Amazon ECR) private repositories.
newPrivateRegistryAccessRequest ::
  PrivateRegistryAccessRequest
newPrivateRegistryAccessRequest =
  PrivateRegistryAccessRequest'
    { ecrImagePullerRole =
        Prelude.Nothing
    }

-- | An object to describe a request to activate or deactivate the role that
-- you can use to grant an Amazon Lightsail container service access to
-- Amazon Elastic Container Registry (Amazon ECR) private repositories.
privateRegistryAccessRequest_ecrImagePullerRole :: Lens.Lens' PrivateRegistryAccessRequest (Prelude.Maybe ContainerServiceECRImagePullerRoleRequest)
privateRegistryAccessRequest_ecrImagePullerRole = Lens.lens (\PrivateRegistryAccessRequest' {ecrImagePullerRole} -> ecrImagePullerRole) (\s@PrivateRegistryAccessRequest' {} a -> s {ecrImagePullerRole = a} :: PrivateRegistryAccessRequest)

instance
  Prelude.Hashable
    PrivateRegistryAccessRequest
  where
  hashWithSalt _salt PrivateRegistryAccessRequest' {..} =
    _salt `Prelude.hashWithSalt` ecrImagePullerRole

instance Prelude.NFData PrivateRegistryAccessRequest where
  rnf PrivateRegistryAccessRequest' {..} =
    Prelude.rnf ecrImagePullerRole

instance Core.ToJSON PrivateRegistryAccessRequest where
  toJSON PrivateRegistryAccessRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ecrImagePullerRole" Core..=)
              Prelude.<$> ecrImagePullerRole
          ]
      )
