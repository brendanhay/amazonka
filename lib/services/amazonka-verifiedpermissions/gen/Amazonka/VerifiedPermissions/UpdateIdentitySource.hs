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
-- Module      : Amazonka.VerifiedPermissions.UpdateIdentitySource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified identity source to use a new identity provider
-- (IdP) source, or to change the mapping of identities from the IdP to a
-- different principal entity type.
module Amazonka.VerifiedPermissions.UpdateIdentitySource
  ( -- * Creating a Request
    UpdateIdentitySource (..),
    newUpdateIdentitySource,

    -- * Request Lenses
    updateIdentitySource_principalEntityType,
    updateIdentitySource_policyStoreId,
    updateIdentitySource_identitySourceId,
    updateIdentitySource_updateConfiguration,

    -- * Destructuring the Response
    UpdateIdentitySourceResponse (..),
    newUpdateIdentitySourceResponse,

    -- * Response Lenses
    updateIdentitySourceResponse_httpStatus,
    updateIdentitySourceResponse_createdDate,
    updateIdentitySourceResponse_identitySourceId,
    updateIdentitySourceResponse_lastUpdatedDate,
    updateIdentitySourceResponse_policyStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newUpdateIdentitySource' smart constructor.
data UpdateIdentitySource = UpdateIdentitySource'
  { -- | Specifies the data type of principals generated for identities
    -- authenticated by the identity source.
    principalEntityType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the policy store that contains the identity source
    -- that you want to update.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the identity source that you want to update.
    identitySourceId :: Prelude.Text,
    -- | Specifies the details required to communicate with the identity provider
    -- (IdP) associated with this identity source.
    --
    -- At this time, the only valid member of this structure is a Amazon
    -- Cognito user pool configuration.
    --
    -- You must specify a @userPoolArn@, and optionally, a @ClientId@.
    updateConfiguration :: UpdateConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentitySource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalEntityType', 'updateIdentitySource_principalEntityType' - Specifies the data type of principals generated for identities
-- authenticated by the identity source.
--
-- 'policyStoreId', 'updateIdentitySource_policyStoreId' - Specifies the ID of the policy store that contains the identity source
-- that you want to update.
--
-- 'identitySourceId', 'updateIdentitySource_identitySourceId' - Specifies the ID of the identity source that you want to update.
--
-- 'updateConfiguration', 'updateIdentitySource_updateConfiguration' - Specifies the details required to communicate with the identity provider
-- (IdP) associated with this identity source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @userPoolArn@, and optionally, a @ClientId@.
newUpdateIdentitySource ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'identitySourceId'
  Prelude.Text ->
  -- | 'updateConfiguration'
  UpdateConfiguration ->
  UpdateIdentitySource
newUpdateIdentitySource
  pPolicyStoreId_
  pIdentitySourceId_
  pUpdateConfiguration_ =
    UpdateIdentitySource'
      { principalEntityType =
          Prelude.Nothing,
        policyStoreId = pPolicyStoreId_,
        identitySourceId = pIdentitySourceId_,
        updateConfiguration = pUpdateConfiguration_
      }

-- | Specifies the data type of principals generated for identities
-- authenticated by the identity source.
updateIdentitySource_principalEntityType :: Lens.Lens' UpdateIdentitySource (Prelude.Maybe Prelude.Text)
updateIdentitySource_principalEntityType = Lens.lens (\UpdateIdentitySource' {principalEntityType} -> principalEntityType) (\s@UpdateIdentitySource' {} a -> s {principalEntityType = a} :: UpdateIdentitySource)

-- | Specifies the ID of the policy store that contains the identity source
-- that you want to update.
updateIdentitySource_policyStoreId :: Lens.Lens' UpdateIdentitySource Prelude.Text
updateIdentitySource_policyStoreId = Lens.lens (\UpdateIdentitySource' {policyStoreId} -> policyStoreId) (\s@UpdateIdentitySource' {} a -> s {policyStoreId = a} :: UpdateIdentitySource)

-- | Specifies the ID of the identity source that you want to update.
updateIdentitySource_identitySourceId :: Lens.Lens' UpdateIdentitySource Prelude.Text
updateIdentitySource_identitySourceId = Lens.lens (\UpdateIdentitySource' {identitySourceId} -> identitySourceId) (\s@UpdateIdentitySource' {} a -> s {identitySourceId = a} :: UpdateIdentitySource)

-- | Specifies the details required to communicate with the identity provider
-- (IdP) associated with this identity source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @userPoolArn@, and optionally, a @ClientId@.
updateIdentitySource_updateConfiguration :: Lens.Lens' UpdateIdentitySource UpdateConfiguration
updateIdentitySource_updateConfiguration = Lens.lens (\UpdateIdentitySource' {updateConfiguration} -> updateConfiguration) (\s@UpdateIdentitySource' {} a -> s {updateConfiguration = a} :: UpdateIdentitySource)

instance Core.AWSRequest UpdateIdentitySource where
  type
    AWSResponse UpdateIdentitySource =
      UpdateIdentitySourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentitySourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "identitySourceId")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
            Prelude.<*> (x Data..:> "policyStoreId")
      )

instance Prelude.Hashable UpdateIdentitySource where
  hashWithSalt _salt UpdateIdentitySource' {..} =
    _salt
      `Prelude.hashWithSalt` principalEntityType
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` identitySourceId
      `Prelude.hashWithSalt` updateConfiguration

instance Prelude.NFData UpdateIdentitySource where
  rnf UpdateIdentitySource' {..} =
    Prelude.rnf principalEntityType
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf identitySourceId
      `Prelude.seq` Prelude.rnf updateConfiguration

instance Data.ToHeaders UpdateIdentitySource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.UpdateIdentitySource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIdentitySource where
  toJSON UpdateIdentitySource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("principalEntityType" Data..=)
              Prelude.<$> principalEntityType,
            Prelude.Just ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("identitySourceId" Data..= identitySourceId),
            Prelude.Just
              ("updateConfiguration" Data..= updateConfiguration)
          ]
      )

instance Data.ToPath UpdateIdentitySource where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateIdentitySource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIdentitySourceResponse' smart constructor.
data UpdateIdentitySourceResponse = UpdateIdentitySourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time that the updated identity source was originally
    -- created.
    createdDate :: Data.ISO8601,
    -- | The ID of the updated identity source.
    identitySourceId :: Prelude.Text,
    -- | The date and time that the identity source was most recently updated.
    lastUpdatedDate :: Data.ISO8601,
    -- | The ID of the policy store that contains the updated identity source.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIdentitySourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIdentitySourceResponse_httpStatus' - The response's http status code.
--
-- 'createdDate', 'updateIdentitySourceResponse_createdDate' - The date and time that the updated identity source was originally
-- created.
--
-- 'identitySourceId', 'updateIdentitySourceResponse_identitySourceId' - The ID of the updated identity source.
--
-- 'lastUpdatedDate', 'updateIdentitySourceResponse_lastUpdatedDate' - The date and time that the identity source was most recently updated.
--
-- 'policyStoreId', 'updateIdentitySourceResponse_policyStoreId' - The ID of the policy store that contains the updated identity source.
newUpdateIdentitySourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'identitySourceId'
  Prelude.Text ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  -- | 'policyStoreId'
  Prelude.Text ->
  UpdateIdentitySourceResponse
newUpdateIdentitySourceResponse
  pHttpStatus_
  pCreatedDate_
  pIdentitySourceId_
  pLastUpdatedDate_
  pPolicyStoreId_ =
    UpdateIdentitySourceResponse'
      { httpStatus =
          pHttpStatus_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        identitySourceId = pIdentitySourceId_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_,
        policyStoreId = pPolicyStoreId_
      }

-- | The response's http status code.
updateIdentitySourceResponse_httpStatus :: Lens.Lens' UpdateIdentitySourceResponse Prelude.Int
updateIdentitySourceResponse_httpStatus = Lens.lens (\UpdateIdentitySourceResponse' {httpStatus} -> httpStatus) (\s@UpdateIdentitySourceResponse' {} a -> s {httpStatus = a} :: UpdateIdentitySourceResponse)

-- | The date and time that the updated identity source was originally
-- created.
updateIdentitySourceResponse_createdDate :: Lens.Lens' UpdateIdentitySourceResponse Prelude.UTCTime
updateIdentitySourceResponse_createdDate = Lens.lens (\UpdateIdentitySourceResponse' {createdDate} -> createdDate) (\s@UpdateIdentitySourceResponse' {} a -> s {createdDate = a} :: UpdateIdentitySourceResponse) Prelude.. Data._Time

-- | The ID of the updated identity source.
updateIdentitySourceResponse_identitySourceId :: Lens.Lens' UpdateIdentitySourceResponse Prelude.Text
updateIdentitySourceResponse_identitySourceId = Lens.lens (\UpdateIdentitySourceResponse' {identitySourceId} -> identitySourceId) (\s@UpdateIdentitySourceResponse' {} a -> s {identitySourceId = a} :: UpdateIdentitySourceResponse)

-- | The date and time that the identity source was most recently updated.
updateIdentitySourceResponse_lastUpdatedDate :: Lens.Lens' UpdateIdentitySourceResponse Prelude.UTCTime
updateIdentitySourceResponse_lastUpdatedDate = Lens.lens (\UpdateIdentitySourceResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@UpdateIdentitySourceResponse' {} a -> s {lastUpdatedDate = a} :: UpdateIdentitySourceResponse) Prelude.. Data._Time

-- | The ID of the policy store that contains the updated identity source.
updateIdentitySourceResponse_policyStoreId :: Lens.Lens' UpdateIdentitySourceResponse Prelude.Text
updateIdentitySourceResponse_policyStoreId = Lens.lens (\UpdateIdentitySourceResponse' {policyStoreId} -> policyStoreId) (\s@UpdateIdentitySourceResponse' {} a -> s {policyStoreId = a} :: UpdateIdentitySourceResponse)

instance Prelude.NFData UpdateIdentitySourceResponse where
  rnf UpdateIdentitySourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf identitySourceId
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf policyStoreId
