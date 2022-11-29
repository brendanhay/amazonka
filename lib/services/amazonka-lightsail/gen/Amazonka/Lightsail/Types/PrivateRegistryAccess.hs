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
-- Module      : Amazonka.Lightsail.Types.PrivateRegistryAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PrivateRegistryAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRole
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for an Amazon Lightsail container service to
-- access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newPrivateRegistryAccess' smart constructor.
data PrivateRegistryAccess = PrivateRegistryAccess'
  { -- | An object that describes the activation status of the role that you can
    -- use to grant a Lightsail container service access to Amazon ECR private
    -- repositories. If the role is activated, the Amazon Resource Name (ARN)
    -- of the role is also listed.
    ecrImagePullerRole :: Prelude.Maybe ContainerServiceECRImagePullerRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateRegistryAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecrImagePullerRole', 'privateRegistryAccess_ecrImagePullerRole' - An object that describes the activation status of the role that you can
-- use to grant a Lightsail container service access to Amazon ECR private
-- repositories. If the role is activated, the Amazon Resource Name (ARN)
-- of the role is also listed.
newPrivateRegistryAccess ::
  PrivateRegistryAccess
newPrivateRegistryAccess =
  PrivateRegistryAccess'
    { ecrImagePullerRole =
        Prelude.Nothing
    }

-- | An object that describes the activation status of the role that you can
-- use to grant a Lightsail container service access to Amazon ECR private
-- repositories. If the role is activated, the Amazon Resource Name (ARN)
-- of the role is also listed.
privateRegistryAccess_ecrImagePullerRole :: Lens.Lens' PrivateRegistryAccess (Prelude.Maybe ContainerServiceECRImagePullerRole)
privateRegistryAccess_ecrImagePullerRole = Lens.lens (\PrivateRegistryAccess' {ecrImagePullerRole} -> ecrImagePullerRole) (\s@PrivateRegistryAccess' {} a -> s {ecrImagePullerRole = a} :: PrivateRegistryAccess)

instance Core.FromJSON PrivateRegistryAccess where
  parseJSON =
    Core.withObject
      "PrivateRegistryAccess"
      ( \x ->
          PrivateRegistryAccess'
            Prelude.<$> (x Core..:? "ecrImagePullerRole")
      )

instance Prelude.Hashable PrivateRegistryAccess where
  hashWithSalt _salt PrivateRegistryAccess' {..} =
    _salt `Prelude.hashWithSalt` ecrImagePullerRole

instance Prelude.NFData PrivateRegistryAccess where
  rnf PrivateRegistryAccess' {..} =
    Prelude.rnf ecrImagePullerRole
