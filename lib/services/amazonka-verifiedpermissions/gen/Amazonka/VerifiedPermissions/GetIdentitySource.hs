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
-- Module      : Amazonka.VerifiedPermissions.GetIdentitySource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details about the specified identity source.
module Amazonka.VerifiedPermissions.GetIdentitySource
  ( -- * Creating a Request
    GetIdentitySource (..),
    newGetIdentitySource,

    -- * Request Lenses
    getIdentitySource_policyStoreId,
    getIdentitySource_identitySourceId,

    -- * Destructuring the Response
    GetIdentitySourceResponse (..),
    newGetIdentitySourceResponse,

    -- * Response Lenses
    getIdentitySourceResponse_httpStatus,
    getIdentitySourceResponse_createdDate,
    getIdentitySourceResponse_details,
    getIdentitySourceResponse_identitySourceId,
    getIdentitySourceResponse_lastUpdatedDate,
    getIdentitySourceResponse_policyStoreId,
    getIdentitySourceResponse_principalEntityType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newGetIdentitySource' smart constructor.
data GetIdentitySource = GetIdentitySource'
  { -- | Specifies the ID of the policy store that contains the identity source
    -- you want information about.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the identity source you want information about.
    identitySourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentitySource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'getIdentitySource_policyStoreId' - Specifies the ID of the policy store that contains the identity source
-- you want information about.
--
-- 'identitySourceId', 'getIdentitySource_identitySourceId' - Specifies the ID of the identity source you want information about.
newGetIdentitySource ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'identitySourceId'
  Prelude.Text ->
  GetIdentitySource
newGetIdentitySource
  pPolicyStoreId_
  pIdentitySourceId_ =
    GetIdentitySource'
      { policyStoreId = pPolicyStoreId_,
        identitySourceId = pIdentitySourceId_
      }

-- | Specifies the ID of the policy store that contains the identity source
-- you want information about.
getIdentitySource_policyStoreId :: Lens.Lens' GetIdentitySource Prelude.Text
getIdentitySource_policyStoreId = Lens.lens (\GetIdentitySource' {policyStoreId} -> policyStoreId) (\s@GetIdentitySource' {} a -> s {policyStoreId = a} :: GetIdentitySource)

-- | Specifies the ID of the identity source you want information about.
getIdentitySource_identitySourceId :: Lens.Lens' GetIdentitySource Prelude.Text
getIdentitySource_identitySourceId = Lens.lens (\GetIdentitySource' {identitySourceId} -> identitySourceId) (\s@GetIdentitySource' {} a -> s {identitySourceId = a} :: GetIdentitySource)

instance Core.AWSRequest GetIdentitySource where
  type
    AWSResponse GetIdentitySource =
      GetIdentitySourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentitySourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "details")
            Prelude.<*> (x Data..:> "identitySourceId")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "principalEntityType")
      )

instance Prelude.Hashable GetIdentitySource where
  hashWithSalt _salt GetIdentitySource' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` identitySourceId

instance Prelude.NFData GetIdentitySource where
  rnf GetIdentitySource' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf identitySourceId

instance Data.ToHeaders GetIdentitySource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.GetIdentitySource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetIdentitySource where
  toJSON GetIdentitySource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("identitySourceId" Data..= identitySourceId)
          ]
      )

instance Data.ToPath GetIdentitySource where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIdentitySource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIdentitySourceResponse' smart constructor.
data GetIdentitySourceResponse = GetIdentitySourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time that the identity source was originally created.
    createdDate :: Data.ISO8601,
    -- | A structure that describes the configuration of the identity source.
    details :: IdentitySourceDetails,
    -- | The ID of the identity source.
    identitySourceId :: Prelude.Text,
    -- | The date and time that the identity source was most recently updated.
    lastUpdatedDate :: Data.ISO8601,
    -- | The ID of the policy store that contains the identity source.
    policyStoreId :: Prelude.Text,
    -- | The data type of principals generated for identities authenticated by
    -- this identity source.
    principalEntityType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentitySourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentitySourceResponse_httpStatus' - The response's http status code.
--
-- 'createdDate', 'getIdentitySourceResponse_createdDate' - The date and time that the identity source was originally created.
--
-- 'details', 'getIdentitySourceResponse_details' - A structure that describes the configuration of the identity source.
--
-- 'identitySourceId', 'getIdentitySourceResponse_identitySourceId' - The ID of the identity source.
--
-- 'lastUpdatedDate', 'getIdentitySourceResponse_lastUpdatedDate' - The date and time that the identity source was most recently updated.
--
-- 'policyStoreId', 'getIdentitySourceResponse_policyStoreId' - The ID of the policy store that contains the identity source.
--
-- 'principalEntityType', 'getIdentitySourceResponse_principalEntityType' - The data type of principals generated for identities authenticated by
-- this identity source.
newGetIdentitySourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'details'
  IdentitySourceDetails ->
  -- | 'identitySourceId'
  Prelude.Text ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'principalEntityType'
  Prelude.Text ->
  GetIdentitySourceResponse
newGetIdentitySourceResponse
  pHttpStatus_
  pCreatedDate_
  pDetails_
  pIdentitySourceId_
  pLastUpdatedDate_
  pPolicyStoreId_
  pPrincipalEntityType_ =
    GetIdentitySourceResponse'
      { httpStatus =
          pHttpStatus_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        details = pDetails_,
        identitySourceId = pIdentitySourceId_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_,
        policyStoreId = pPolicyStoreId_,
        principalEntityType = pPrincipalEntityType_
      }

-- | The response's http status code.
getIdentitySourceResponse_httpStatus :: Lens.Lens' GetIdentitySourceResponse Prelude.Int
getIdentitySourceResponse_httpStatus = Lens.lens (\GetIdentitySourceResponse' {httpStatus} -> httpStatus) (\s@GetIdentitySourceResponse' {} a -> s {httpStatus = a} :: GetIdentitySourceResponse)

-- | The date and time that the identity source was originally created.
getIdentitySourceResponse_createdDate :: Lens.Lens' GetIdentitySourceResponse Prelude.UTCTime
getIdentitySourceResponse_createdDate = Lens.lens (\GetIdentitySourceResponse' {createdDate} -> createdDate) (\s@GetIdentitySourceResponse' {} a -> s {createdDate = a} :: GetIdentitySourceResponse) Prelude.. Data._Time

-- | A structure that describes the configuration of the identity source.
getIdentitySourceResponse_details :: Lens.Lens' GetIdentitySourceResponse IdentitySourceDetails
getIdentitySourceResponse_details = Lens.lens (\GetIdentitySourceResponse' {details} -> details) (\s@GetIdentitySourceResponse' {} a -> s {details = a} :: GetIdentitySourceResponse)

-- | The ID of the identity source.
getIdentitySourceResponse_identitySourceId :: Lens.Lens' GetIdentitySourceResponse Prelude.Text
getIdentitySourceResponse_identitySourceId = Lens.lens (\GetIdentitySourceResponse' {identitySourceId} -> identitySourceId) (\s@GetIdentitySourceResponse' {} a -> s {identitySourceId = a} :: GetIdentitySourceResponse)

-- | The date and time that the identity source was most recently updated.
getIdentitySourceResponse_lastUpdatedDate :: Lens.Lens' GetIdentitySourceResponse Prelude.UTCTime
getIdentitySourceResponse_lastUpdatedDate = Lens.lens (\GetIdentitySourceResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetIdentitySourceResponse' {} a -> s {lastUpdatedDate = a} :: GetIdentitySourceResponse) Prelude.. Data._Time

-- | The ID of the policy store that contains the identity source.
getIdentitySourceResponse_policyStoreId :: Lens.Lens' GetIdentitySourceResponse Prelude.Text
getIdentitySourceResponse_policyStoreId = Lens.lens (\GetIdentitySourceResponse' {policyStoreId} -> policyStoreId) (\s@GetIdentitySourceResponse' {} a -> s {policyStoreId = a} :: GetIdentitySourceResponse)

-- | The data type of principals generated for identities authenticated by
-- this identity source.
getIdentitySourceResponse_principalEntityType :: Lens.Lens' GetIdentitySourceResponse Prelude.Text
getIdentitySourceResponse_principalEntityType = Lens.lens (\GetIdentitySourceResponse' {principalEntityType} -> principalEntityType) (\s@GetIdentitySourceResponse' {} a -> s {principalEntityType = a} :: GetIdentitySourceResponse)

instance Prelude.NFData GetIdentitySourceResponse where
  rnf GetIdentitySourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf identitySourceId
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf principalEntityType
