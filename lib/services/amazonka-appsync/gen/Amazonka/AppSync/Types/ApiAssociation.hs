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
-- Module      : Amazonka.AppSync.Types.ApiAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ApiAssociation where

import Amazonka.AppSync.Types.AssociationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an @ApiAssociation@ object.
--
-- /See:/ 'newApiAssociation' smart constructor.
data ApiAssociation = ApiAssociation'
  { -- | The domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the status of an association.
    --
    -- -   __PROCESSING__: The API association is being created. You cannot
    --     modify association requests during processing.
    --
    -- -   __SUCCESS__: The API association was successful. You can modify
    --     associations after success.
    --
    -- -   __FAILED__: The API association has failed. You can modify
    --     associations after failure.
    associationStatus :: Prelude.Maybe AssociationStatus,
    -- | Details about the last deployment status.
    deploymentDetail :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'apiAssociation_domainName' - The domain name.
--
-- 'apiId', 'apiAssociation_apiId' - The API ID.
--
-- 'associationStatus', 'apiAssociation_associationStatus' - Identifies the status of an association.
--
-- -   __PROCESSING__: The API association is being created. You cannot
--     modify association requests during processing.
--
-- -   __SUCCESS__: The API association was successful. You can modify
--     associations after success.
--
-- -   __FAILED__: The API association has failed. You can modify
--     associations after failure.
--
-- 'deploymentDetail', 'apiAssociation_deploymentDetail' - Details about the last deployment status.
newApiAssociation ::
  ApiAssociation
newApiAssociation =
  ApiAssociation'
    { domainName = Prelude.Nothing,
      apiId = Prelude.Nothing,
      associationStatus = Prelude.Nothing,
      deploymentDetail = Prelude.Nothing
    }

-- | The domain name.
apiAssociation_domainName :: Lens.Lens' ApiAssociation (Prelude.Maybe Prelude.Text)
apiAssociation_domainName = Lens.lens (\ApiAssociation' {domainName} -> domainName) (\s@ApiAssociation' {} a -> s {domainName = a} :: ApiAssociation)

-- | The API ID.
apiAssociation_apiId :: Lens.Lens' ApiAssociation (Prelude.Maybe Prelude.Text)
apiAssociation_apiId = Lens.lens (\ApiAssociation' {apiId} -> apiId) (\s@ApiAssociation' {} a -> s {apiId = a} :: ApiAssociation)

-- | Identifies the status of an association.
--
-- -   __PROCESSING__: The API association is being created. You cannot
--     modify association requests during processing.
--
-- -   __SUCCESS__: The API association was successful. You can modify
--     associations after success.
--
-- -   __FAILED__: The API association has failed. You can modify
--     associations after failure.
apiAssociation_associationStatus :: Lens.Lens' ApiAssociation (Prelude.Maybe AssociationStatus)
apiAssociation_associationStatus = Lens.lens (\ApiAssociation' {associationStatus} -> associationStatus) (\s@ApiAssociation' {} a -> s {associationStatus = a} :: ApiAssociation)

-- | Details about the last deployment status.
apiAssociation_deploymentDetail :: Lens.Lens' ApiAssociation (Prelude.Maybe Prelude.Text)
apiAssociation_deploymentDetail = Lens.lens (\ApiAssociation' {deploymentDetail} -> deploymentDetail) (\s@ApiAssociation' {} a -> s {deploymentDetail = a} :: ApiAssociation)

instance Core.FromJSON ApiAssociation where
  parseJSON =
    Core.withObject
      "ApiAssociation"
      ( \x ->
          ApiAssociation'
            Prelude.<$> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "apiId")
            Prelude.<*> (x Core..:? "associationStatus")
            Prelude.<*> (x Core..:? "deploymentDetail")
      )

instance Prelude.Hashable ApiAssociation where
  hashWithSalt _salt ApiAssociation' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` associationStatus
      `Prelude.hashWithSalt` deploymentDetail

instance Prelude.NFData ApiAssociation where
  rnf ApiAssociation' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf associationStatus
      `Prelude.seq` Prelude.rnf deploymentDetail
