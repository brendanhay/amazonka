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
-- Module      : Amazonka.CustomerProfiles.AddProfileKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a new key value with a specific profile, such as a Contact
-- Record ContactId.
--
-- A profile object can have a single unique key and any number of
-- additional keys that can be used to identify the profile that it belongs
-- to.
module Amazonka.CustomerProfiles.AddProfileKey
  ( -- * Creating a Request
    AddProfileKey (..),
    newAddProfileKey,

    -- * Request Lenses
    addProfileKey_profileId,
    addProfileKey_keyName,
    addProfileKey_values,
    addProfileKey_domainName,

    -- * Destructuring the Response
    AddProfileKeyResponse (..),
    newAddProfileKeyResponse,

    -- * Response Lenses
    addProfileKeyResponse_keyName,
    addProfileKeyResponse_values,
    addProfileKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddProfileKey' smart constructor.
data AddProfileKey = AddProfileKey'
  { -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text,
    -- | A searchable identifier of a customer profile. The predefined keys you
    -- can use include: _account, _profileId, _assetId, _caseId, _orderId,
    -- _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
    -- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
    -- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
    -- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
    -- _shopifyCustomerId, _shopifyOrderId.
    keyName :: Prelude.Text,
    -- | A list of key values.
    values :: [Prelude.Text],
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddProfileKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'addProfileKey_profileId' - The unique identifier of a customer profile.
--
-- 'keyName', 'addProfileKey_keyName' - A searchable identifier of a customer profile. The predefined keys you
-- can use include: _account, _profileId, _assetId, _caseId, _orderId,
-- _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
-- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
-- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
-- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
-- _shopifyCustomerId, _shopifyOrderId.
--
-- 'values', 'addProfileKey_values' - A list of key values.
--
-- 'domainName', 'addProfileKey_domainName' - The unique name of the domain.
newAddProfileKey ::
  -- | 'profileId'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  AddProfileKey
newAddProfileKey pProfileId_ pKeyName_ pDomainName_ =
  AddProfileKey'
    { profileId = pProfileId_,
      keyName = pKeyName_,
      values = Prelude.mempty,
      domainName = pDomainName_
    }

-- | The unique identifier of a customer profile.
addProfileKey_profileId :: Lens.Lens' AddProfileKey Prelude.Text
addProfileKey_profileId = Lens.lens (\AddProfileKey' {profileId} -> profileId) (\s@AddProfileKey' {} a -> s {profileId = a} :: AddProfileKey)

-- | A searchable identifier of a customer profile. The predefined keys you
-- can use include: _account, _profileId, _assetId, _caseId, _orderId,
-- _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
-- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
-- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
-- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
-- _shopifyCustomerId, _shopifyOrderId.
addProfileKey_keyName :: Lens.Lens' AddProfileKey Prelude.Text
addProfileKey_keyName = Lens.lens (\AddProfileKey' {keyName} -> keyName) (\s@AddProfileKey' {} a -> s {keyName = a} :: AddProfileKey)

-- | A list of key values.
addProfileKey_values :: Lens.Lens' AddProfileKey [Prelude.Text]
addProfileKey_values = Lens.lens (\AddProfileKey' {values} -> values) (\s@AddProfileKey' {} a -> s {values = a} :: AddProfileKey) Prelude.. Lens.coerced

-- | The unique name of the domain.
addProfileKey_domainName :: Lens.Lens' AddProfileKey Prelude.Text
addProfileKey_domainName = Lens.lens (\AddProfileKey' {domainName} -> domainName) (\s@AddProfileKey' {} a -> s {domainName = a} :: AddProfileKey)

instance Core.AWSRequest AddProfileKey where
  type
    AWSResponse AddProfileKey =
      AddProfileKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddProfileKeyResponse'
            Prelude.<$> (x Data..?> "KeyName")
            Prelude.<*> (x Data..?> "Values" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddProfileKey where
  hashWithSalt _salt AddProfileKey' {..} =
    _salt
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData AddProfileKey where
  rnf AddProfileKey' {..} =
    Prelude.rnf profileId `Prelude.seq`
      Prelude.rnf keyName `Prelude.seq`
        Prelude.rnf values `Prelude.seq`
          Prelude.rnf domainName

instance Data.ToHeaders AddProfileKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddProfileKey where
  toJSON AddProfileKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProfileId" Data..= profileId),
            Prelude.Just ("KeyName" Data..= keyName),
            Prelude.Just ("Values" Data..= values)
          ]
      )

instance Data.ToPath AddProfileKey where
  toPath AddProfileKey' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/profiles/keys"]

instance Data.ToQuery AddProfileKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddProfileKeyResponse' smart constructor.
data AddProfileKeyResponse = AddProfileKeyResponse'
  { -- | A searchable identifier of a customer profile.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | A list of key values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddProfileKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'addProfileKeyResponse_keyName' - A searchable identifier of a customer profile.
--
-- 'values', 'addProfileKeyResponse_values' - A list of key values.
--
-- 'httpStatus', 'addProfileKeyResponse_httpStatus' - The response's http status code.
newAddProfileKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddProfileKeyResponse
newAddProfileKeyResponse pHttpStatus_ =
  AddProfileKeyResponse'
    { keyName = Prelude.Nothing,
      values = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A searchable identifier of a customer profile.
addProfileKeyResponse_keyName :: Lens.Lens' AddProfileKeyResponse (Prelude.Maybe Prelude.Text)
addProfileKeyResponse_keyName = Lens.lens (\AddProfileKeyResponse' {keyName} -> keyName) (\s@AddProfileKeyResponse' {} a -> s {keyName = a} :: AddProfileKeyResponse)

-- | A list of key values.
addProfileKeyResponse_values :: Lens.Lens' AddProfileKeyResponse (Prelude.Maybe [Prelude.Text])
addProfileKeyResponse_values = Lens.lens (\AddProfileKeyResponse' {values} -> values) (\s@AddProfileKeyResponse' {} a -> s {values = a} :: AddProfileKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addProfileKeyResponse_httpStatus :: Lens.Lens' AddProfileKeyResponse Prelude.Int
addProfileKeyResponse_httpStatus = Lens.lens (\AddProfileKeyResponse' {httpStatus} -> httpStatus) (\s@AddProfileKeyResponse' {} a -> s {httpStatus = a} :: AddProfileKeyResponse)

instance Prelude.NFData AddProfileKeyResponse where
  rnf AddProfileKeyResponse' {..} =
    Prelude.rnf keyName `Prelude.seq`
      Prelude.rnf values `Prelude.seq`
        Prelude.rnf httpStatus
