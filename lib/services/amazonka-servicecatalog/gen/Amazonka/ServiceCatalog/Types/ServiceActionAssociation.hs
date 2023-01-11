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
-- Module      : Amazonka.ServiceCatalog.Types.ServiceActionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ServiceActionAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A self-service action association consisting of the Action ID, the
-- Product ID, and the Provisioning Artifact ID.
--
-- /See:/ 'newServiceActionAssociation' smart constructor.
data ServiceActionAssociation = ServiceActionAssociation'
  { -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Prelude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Prelude.Text,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceActionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionId', 'serviceActionAssociation_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
--
-- 'productId', 'serviceActionAssociation_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'provisioningArtifactId', 'serviceActionAssociation_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
newServiceActionAssociation ::
  -- | 'serviceActionId'
  Prelude.Text ->
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  ServiceActionAssociation
newServiceActionAssociation
  pServiceActionId_
  pProductId_
  pProvisioningArtifactId_ =
    ServiceActionAssociation'
      { serviceActionId =
          pServiceActionId_,
        productId = pProductId_,
        provisioningArtifactId = pProvisioningArtifactId_
      }

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
serviceActionAssociation_serviceActionId :: Lens.Lens' ServiceActionAssociation Prelude.Text
serviceActionAssociation_serviceActionId = Lens.lens (\ServiceActionAssociation' {serviceActionId} -> serviceActionId) (\s@ServiceActionAssociation' {} a -> s {serviceActionId = a} :: ServiceActionAssociation)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
serviceActionAssociation_productId :: Lens.Lens' ServiceActionAssociation Prelude.Text
serviceActionAssociation_productId = Lens.lens (\ServiceActionAssociation' {productId} -> productId) (\s@ServiceActionAssociation' {} a -> s {productId = a} :: ServiceActionAssociation)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
serviceActionAssociation_provisioningArtifactId :: Lens.Lens' ServiceActionAssociation Prelude.Text
serviceActionAssociation_provisioningArtifactId = Lens.lens (\ServiceActionAssociation' {provisioningArtifactId} -> provisioningArtifactId) (\s@ServiceActionAssociation' {} a -> s {provisioningArtifactId = a} :: ServiceActionAssociation)

instance Prelude.Hashable ServiceActionAssociation where
  hashWithSalt _salt ServiceActionAssociation' {..} =
    _salt `Prelude.hashWithSalt` serviceActionId
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` provisioningArtifactId

instance Prelude.NFData ServiceActionAssociation where
  rnf ServiceActionAssociation' {..} =
    Prelude.rnf serviceActionId
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf provisioningArtifactId

instance Data.ToJSON ServiceActionAssociation where
  toJSON ServiceActionAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ServiceActionId" Data..= serviceActionId),
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Data..= provisioningArtifactId
              )
          ]
      )
