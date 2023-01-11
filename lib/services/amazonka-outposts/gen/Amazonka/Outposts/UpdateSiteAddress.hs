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
-- Module      : Amazonka.Outposts.UpdateSiteAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the address of the specified site.
--
-- You can\'t update a site address if there is an order in progress. You
-- must wait for the order to complete or cancel the order.
--
-- You can update the operating address before you place an order at the
-- site, or after all Outposts that belong to the site have been
-- deactivated.
module Amazonka.Outposts.UpdateSiteAddress
  ( -- * Creating a Request
    UpdateSiteAddress (..),
    newUpdateSiteAddress,

    -- * Request Lenses
    updateSiteAddress_siteId,
    updateSiteAddress_addressType,
    updateSiteAddress_address,

    -- * Destructuring the Response
    UpdateSiteAddressResponse (..),
    newUpdateSiteAddressResponse,

    -- * Response Lenses
    updateSiteAddressResponse_address,
    updateSiteAddressResponse_addressType,
    updateSiteAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSiteAddress' smart constructor.
data UpdateSiteAddress = UpdateSiteAddress'
  { -- | The ID or the Amazon Resource Name (ARN) of the site.
    siteId :: Prelude.Text,
    -- | The type of the address.
    addressType :: AddressType,
    -- | The address for the site.
    address :: Address
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteId', 'updateSiteAddress_siteId' - The ID or the Amazon Resource Name (ARN) of the site.
--
-- 'addressType', 'updateSiteAddress_addressType' - The type of the address.
--
-- 'address', 'updateSiteAddress_address' - The address for the site.
newUpdateSiteAddress ::
  -- | 'siteId'
  Prelude.Text ->
  -- | 'addressType'
  AddressType ->
  -- | 'address'
  Address ->
  UpdateSiteAddress
newUpdateSiteAddress pSiteId_ pAddressType_ pAddress_ =
  UpdateSiteAddress'
    { siteId = pSiteId_,
      addressType = pAddressType_,
      address = pAddress_
    }

-- | The ID or the Amazon Resource Name (ARN) of the site.
updateSiteAddress_siteId :: Lens.Lens' UpdateSiteAddress Prelude.Text
updateSiteAddress_siteId = Lens.lens (\UpdateSiteAddress' {siteId} -> siteId) (\s@UpdateSiteAddress' {} a -> s {siteId = a} :: UpdateSiteAddress)

-- | The type of the address.
updateSiteAddress_addressType :: Lens.Lens' UpdateSiteAddress AddressType
updateSiteAddress_addressType = Lens.lens (\UpdateSiteAddress' {addressType} -> addressType) (\s@UpdateSiteAddress' {} a -> s {addressType = a} :: UpdateSiteAddress)

-- | The address for the site.
updateSiteAddress_address :: Lens.Lens' UpdateSiteAddress Address
updateSiteAddress_address = Lens.lens (\UpdateSiteAddress' {address} -> address) (\s@UpdateSiteAddress' {} a -> s {address = a} :: UpdateSiteAddress)

instance Core.AWSRequest UpdateSiteAddress where
  type
    AWSResponse UpdateSiteAddress =
      UpdateSiteAddressResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteAddressResponse'
            Prelude.<$> (x Data..?> "Address")
            Prelude.<*> (x Data..?> "AddressType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSiteAddress where
  hashWithSalt _salt UpdateSiteAddress' {..} =
    _salt `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` addressType
      `Prelude.hashWithSalt` address

instance Prelude.NFData UpdateSiteAddress where
  rnf UpdateSiteAddress' {..} =
    Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf addressType
      `Prelude.seq` Prelude.rnf address

instance Data.ToHeaders UpdateSiteAddress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSiteAddress where
  toJSON UpdateSiteAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AddressType" Data..= addressType),
            Prelude.Just ("Address" Data..= address)
          ]
      )

instance Data.ToPath UpdateSiteAddress where
  toPath UpdateSiteAddress' {..} =
    Prelude.mconcat
      ["/sites/", Data.toBS siteId, "/address"]

instance Data.ToQuery UpdateSiteAddress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSiteAddressResponse' smart constructor.
data UpdateSiteAddressResponse = UpdateSiteAddressResponse'
  { -- | Information about an address.
    address :: Prelude.Maybe Address,
    -- | The type of the address.
    addressType :: Prelude.Maybe AddressType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'updateSiteAddressResponse_address' - Information about an address.
--
-- 'addressType', 'updateSiteAddressResponse_addressType' - The type of the address.
--
-- 'httpStatus', 'updateSiteAddressResponse_httpStatus' - The response's http status code.
newUpdateSiteAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSiteAddressResponse
newUpdateSiteAddressResponse pHttpStatus_ =
  UpdateSiteAddressResponse'
    { address =
        Prelude.Nothing,
      addressType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about an address.
updateSiteAddressResponse_address :: Lens.Lens' UpdateSiteAddressResponse (Prelude.Maybe Address)
updateSiteAddressResponse_address = Lens.lens (\UpdateSiteAddressResponse' {address} -> address) (\s@UpdateSiteAddressResponse' {} a -> s {address = a} :: UpdateSiteAddressResponse)

-- | The type of the address.
updateSiteAddressResponse_addressType :: Lens.Lens' UpdateSiteAddressResponse (Prelude.Maybe AddressType)
updateSiteAddressResponse_addressType = Lens.lens (\UpdateSiteAddressResponse' {addressType} -> addressType) (\s@UpdateSiteAddressResponse' {} a -> s {addressType = a} :: UpdateSiteAddressResponse)

-- | The response's http status code.
updateSiteAddressResponse_httpStatus :: Lens.Lens' UpdateSiteAddressResponse Prelude.Int
updateSiteAddressResponse_httpStatus = Lens.lens (\UpdateSiteAddressResponse' {httpStatus} -> httpStatus) (\s@UpdateSiteAddressResponse' {} a -> s {httpStatus = a} :: UpdateSiteAddressResponse)

instance Prelude.NFData UpdateSiteAddressResponse where
  rnf UpdateSiteAddressResponse' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf addressType
      `Prelude.seq` Prelude.rnf httpStatus
