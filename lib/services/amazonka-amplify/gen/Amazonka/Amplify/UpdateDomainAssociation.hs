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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain association for an Amplify app.
module Amazonka.Amplify.UpdateDomainAssociation
  ( -- * Creating a Request
    UpdateDomainAssociation (..),
    newUpdateDomainAssociation,

    -- * Request Lenses
    updateDomainAssociation_autoSubDomainCreationPatterns,
    updateDomainAssociation_autoSubDomainIAMRole,
    updateDomainAssociation_enableAutoSubDomain,
    updateDomainAssociation_subDomainSettings,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the update domain association request.
--
-- /See:/ 'newUpdateDomainAssociation' smart constructor.
data UpdateDomainAssociation = UpdateDomainAssociation'
  { -- | Sets the branch patterns for automatic subdomain creation.
    autoSubDomainCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The required AWS Identity and Access Management (IAM) service role for
    -- the Amazon Resource Name (ARN) for automatically creating subdomains.
    autoSubDomainIAMRole :: Prelude.Maybe Prelude.Text,
    -- | Enables the automated creation of subdomains for branches.
    enableAutoSubDomain :: Prelude.Maybe Prelude.Bool,
    -- | Describes the settings for the subdomain.
    subDomainSettings :: Prelude.Maybe [SubDomainSetting],
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
-- 'autoSubDomainCreationPatterns', 'updateDomainAssociation_autoSubDomainCreationPatterns' - Sets the branch patterns for automatic subdomain creation.
--
-- 'autoSubDomainIAMRole', 'updateDomainAssociation_autoSubDomainIAMRole' - The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
--
-- 'enableAutoSubDomain', 'updateDomainAssociation_enableAutoSubDomain' - Enables the automated creation of subdomains for branches.
--
-- 'subDomainSettings', 'updateDomainAssociation_subDomainSettings' - Describes the settings for the subdomain.
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
    { autoSubDomainCreationPatterns =
        Prelude.Nothing,
      autoSubDomainIAMRole = Prelude.Nothing,
      enableAutoSubDomain = Prelude.Nothing,
      subDomainSettings = Prelude.Nothing,
      appId = pAppId_,
      domainName = pDomainName_
    }

-- | Sets the branch patterns for automatic subdomain creation.
updateDomainAssociation_autoSubDomainCreationPatterns :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe [Prelude.Text])
updateDomainAssociation_autoSubDomainCreationPatterns = Lens.lens (\UpdateDomainAssociation' {autoSubDomainCreationPatterns} -> autoSubDomainCreationPatterns) (\s@UpdateDomainAssociation' {} a -> s {autoSubDomainCreationPatterns = a} :: UpdateDomainAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
updateDomainAssociation_autoSubDomainIAMRole :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe Prelude.Text)
updateDomainAssociation_autoSubDomainIAMRole = Lens.lens (\UpdateDomainAssociation' {autoSubDomainIAMRole} -> autoSubDomainIAMRole) (\s@UpdateDomainAssociation' {} a -> s {autoSubDomainIAMRole = a} :: UpdateDomainAssociation)

-- | Enables the automated creation of subdomains for branches.
updateDomainAssociation_enableAutoSubDomain :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe Prelude.Bool)
updateDomainAssociation_enableAutoSubDomain = Lens.lens (\UpdateDomainAssociation' {enableAutoSubDomain} -> enableAutoSubDomain) (\s@UpdateDomainAssociation' {} a -> s {enableAutoSubDomain = a} :: UpdateDomainAssociation)

-- | Describes the settings for the subdomain.
updateDomainAssociation_subDomainSettings :: Lens.Lens' UpdateDomainAssociation (Prelude.Maybe [SubDomainSetting])
updateDomainAssociation_subDomainSettings = Lens.lens (\UpdateDomainAssociation' {subDomainSettings} -> subDomainSettings) (\s@UpdateDomainAssociation' {} a -> s {subDomainSettings = a} :: UpdateDomainAssociation) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<*> (x Data..:> "domainAssociation")
      )

instance Prelude.Hashable UpdateDomainAssociation where
  hashWithSalt _salt UpdateDomainAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` autoSubDomainCreationPatterns
      `Prelude.hashWithSalt` autoSubDomainIAMRole
      `Prelude.hashWithSalt` enableAutoSubDomain
      `Prelude.hashWithSalt` subDomainSettings
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainAssociation where
  rnf UpdateDomainAssociation' {..} =
    Prelude.rnf autoSubDomainCreationPatterns
      `Prelude.seq` Prelude.rnf autoSubDomainIAMRole
      `Prelude.seq` Prelude.rnf enableAutoSubDomain
      `Prelude.seq` Prelude.rnf subDomainSettings
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomainAssociation where
  toJSON UpdateDomainAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoSubDomainCreationPatterns" Data..=)
              Prelude.<$> autoSubDomainCreationPatterns,
            ("autoSubDomainIAMRole" Data..=)
              Prelude.<$> autoSubDomainIAMRole,
            ("enableAutoSubDomain" Data..=)
              Prelude.<$> enableAutoSubDomain,
            ("subDomainSettings" Data..=)
              Prelude.<$> subDomainSettings
          ]
      )

instance Data.ToPath UpdateDomainAssociation where
  toPath UpdateDomainAssociation' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/domains/",
        Data.toBS domainName
      ]

instance Data.ToQuery UpdateDomainAssociation where
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
