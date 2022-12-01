{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.UpdateDomainAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain association for an Amplify app.
module Amazonka.Amplify.UpdateDomainAssociation
  ( -- * Creating a Request
    UpdateDomainAssociation (..),
    newUpdateDomainAssociation,

    -- * Request Lenses
    updateDomainAssociation_subDomainSettings,
    updateDomainAssociation_autoSubDomainIAMRole,
    updateDomainAssociation_autoSubDomainCreationPatterns,
    updateDomainAssociation_enableAutoSubDomain,
    updateDomainAssociation_appId,
    updateDomainAssociation_domainName,

    -- * Destructuring the Response
    UpdateDomainAssociationResponse (..),
    newUpdateDomainAssociationResponse,

    -- * Response Lenses
    updateDomainAssociationResponse_httpStatus,
    updateDomainAssociationResponse_domainAssociation,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the update domain association request.
--
-- /See:/ 'newUpdateDomainAssociation' smart constructor.
data UpdateDomainAssociation = UpdateDomainAssociation'
  { -- | Describes the settings for the subdomain.
    subDomainSettings :: Prelude.Maybe [SubDomainSetting],
    -- | The required AWS Identity and Access Management (IAM) service role for
    -- the Amazon Resource Name (ARN) for automatically creating subdomains.
    autoSubDomainIAMRole :: Prelude.Maybe Prelude.Text,
    -- | Sets the branch patterns for automatic subdomain creation.
    autoSubDomainCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Enables the automated creation of subdomains for branches.
    enableAutoSubDomain :: Prelude.Maybe Prelude.Bool,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subDomainSettings', 'updateDomainAssociation_subDomainSettings' - Describes the settings for the subdomain.
--
-- 'autoSubDomainIAMRole', 'updateDomainAssociation_autoSubDomainIAMRole' - The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
--
-- 'autoSubDomainCreationPatterns', 'updateDomainAssociation_autoSubDomainCreationPatterns' - Sets the branch patterns for automatic subdomain creation.
--
-- 'enableAutoSubDomain', 'updateDomainAssociation_enableAutoSubDomain' - Enables the automated creation of subdomains for branches.
--
-- 'appId', 'updateDomainAssociation_appId' - The unique ID for an Amplify app.
--
-- 'domainName', 'updateDomainAssociation_domainName' - The name of the domain.
newUpdateDomainAssociation ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainAssociation
newUpdateDomainAssociation pAppId_ pDomainName_ =
  UpdateDomainAssociation'
    { subDomainSettings =
        Prelude.Nothing,
      autoSubDomainIAMRole = Prelude.Nothing,
      autoSubDomainCreationPatterns = Prelude.Nothing,
      enableAutoSubDomain = Prelude.Nothing,
      appId = pAppId_,
      domainName = pDomainName_
    }

-- | Describes the settings for the subdomain.
updateDomainAssociation_subDomainSettings :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe [SubDomainSetting])
updateDomainAssociation_subDomainSettings = Lens.lens (\UpdateDomainAssociation' {subDomainSettings} -> subDomainSettings) (\s@UpdateDomainAssociation' {} a -> s {subDomainSettings = a} :: UpdateDomainAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
updateDomainAssociation_autoSubDomainIAMRole :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe Prelude.Text)
updateDomainAssociation_autoSubDomainIAMRole = Lens.lens (\UpdateDomainAssociation' {autoSubDomainIAMRole} -> autoSubDomainIAMRole) (\s@UpdateDomainAssociation' {} a -> s {autoSubDomainIAMRole = a} :: UpdateDomainAssociation)

-- | Sets the branch patterns for automatic subdomain creation.
updateDomainAssociation_autoSubDomainCreationPatterns :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe [Prelude.Text])
updateDomainAssociation_autoSubDomainCreationPatterns = Lens.lens (\UpdateDomainAssociation' {autoSubDomainCreationPatterns} -> autoSubDomainCreationPatterns) (\s@UpdateDomainAssociation' {} a -> s {autoSubDomainCreationPatterns = a} :: UpdateDomainAssociation) Prelude.. Lens.mapping Lens.coerced

-- | Enables the automated creation of subdomains for branches.
updateDomainAssociation_enableAutoSubDomain :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe Prelude.Bool)
updateDomainAssociation_enableAutoSubDomain = Lens.lens (\UpdateDomainAssociation' {enableAutoSubDomain} -> enableAutoSubDomain) (\s@UpdateDomainAssociation' {} a -> s {enableAutoSubDomain = a} :: UpdateDomainAssociation)

-- | The unique ID for an Amplify app.
updateDomainAssociation_appId :: Lens.Lens' UpdateDomainAssociation Prelude.Text
updateDomainAssociation_appId = Lens.lens (\UpdateDomainAssociation' {appId} -> appId) (\s@UpdateDomainAssociation' {} a -> s {appId = a} :: UpdateDomainAssociation)

-- | The name of the domain.
updateDomainAssociation_domainName :: Lens.Lens' UpdateDomainAssociation Prelude.Text
updateDomainAssociation_domainName = Lens.lens (\UpdateDomainAssociation' {domainName} -> domainName) (\s@UpdateDomainAssociation' {} a -> s {domainName = a} :: UpdateDomainAssociation)

instance Core.AWSRequest UpdateDomainAssociation where
  type
    AWSResponse UpdateDomainAssociation =
      UpdateDomainAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "domainAssociation")
      )

instance Prelude.Hashable UpdateDomainAssociation where
  hashWithSalt _salt UpdateDomainAssociation' {..} =
    _salt `Prelude.hashWithSalt` subDomainSettings
      `Prelude.hashWithSalt` autoSubDomainIAMRole
      `Prelude.hashWithSalt` autoSubDomainCreationPatterns
      `Prelude.hashWithSalt` enableAutoSubDomain
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainAssociation where
  rnf UpdateDomainAssociation' {..} =
    Prelude.rnf subDomainSettings
      `Prelude.seq` Prelude.rnf autoSubDomainIAMRole
      `Prelude.seq` Prelude.rnf autoSubDomainCreationPatterns
      `Prelude.seq` Prelude.rnf enableAutoSubDomain
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders UpdateDomainAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDomainAssociation where
  toJSON UpdateDomainAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("subDomainSettings" Core..=)
              Prelude.<$> subDomainSettings,
            ("autoSubDomainIAMRole" Core..=)
              Prelude.<$> autoSubDomainIAMRole,
            ("autoSubDomainCreationPatterns" Core..=)
              Prelude.<$> autoSubDomainCreationPatterns,
            ("enableAutoSubDomain" Core..=)
              Prelude.<$> enableAutoSubDomain
          ]
      )

instance Core.ToPath UpdateDomainAssociation where
  toPath UpdateDomainAssociation' {..} =
    Prelude.mconcat
      [ "/apps/",
        Core.toBS appId,
        "/domains/",
        Core.toBS domainName
      ]

instance Core.ToQuery UpdateDomainAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the update domain association request.
--
-- /See:/ 'newUpdateDomainAssociationResponse' smart constructor.
data UpdateDomainAssociationResponse = UpdateDomainAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes a domain association, which associates a custom domain with an
    -- Amplify app.
    domainAssociation :: DomainAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainAssociationResponse_httpStatus' - The response's http status code.
--
-- 'domainAssociation', 'updateDomainAssociationResponse_domainAssociation' - Describes a domain association, which associates a custom domain with an
-- Amplify app.
newUpdateDomainAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainAssociation'
  DomainAssociation ->
  UpdateDomainAssociationResponse
newUpdateDomainAssociationResponse
  pHttpStatus_
  pDomainAssociation_ =
    UpdateDomainAssociationResponse'
      { httpStatus =
          pHttpStatus_,
        domainAssociation = pDomainAssociation_
      }

-- | The response's http status code.
updateDomainAssociationResponse_httpStatus :: Lens.Lens' UpdateDomainAssociationResponse Prelude.Int
updateDomainAssociationResponse_httpStatus = Lens.lens (\UpdateDomainAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainAssociationResponse' {} a -> s {httpStatus = a} :: UpdateDomainAssociationResponse)

-- | Describes a domain association, which associates a custom domain with an
-- Amplify app.
updateDomainAssociationResponse_domainAssociation :: Lens.Lens' UpdateDomainAssociationResponse DomainAssociation
updateDomainAssociationResponse_domainAssociation = Lens.lens (\UpdateDomainAssociationResponse' {domainAssociation} -> domainAssociation) (\s@UpdateDomainAssociationResponse' {} a -> s {domainAssociation = a} :: UpdateDomainAssociationResponse)

instance
  Prelude.NFData
    UpdateDomainAssociationResponse
  where
  rnf UpdateDomainAssociationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainAssociation
