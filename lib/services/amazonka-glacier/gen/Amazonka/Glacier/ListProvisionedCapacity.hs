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
-- Module      : Amazonka.Glacier.ListProvisionedCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the provisioned capacity units for the specified
-- AWS account.
module Amazonka.Glacier.ListProvisionedCapacity
  ( -- * Creating a Request
    ListProvisionedCapacity (..),
    newListProvisionedCapacity,

    -- * Request Lenses
    listProvisionedCapacity_accountId,

    -- * Destructuring the Response
    ListProvisionedCapacityResponse (..),
    newListProvisionedCapacityResponse,

    -- * Response Lenses
    listProvisionedCapacityResponse_provisionedCapacityList,
    listProvisionedCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProvisionedCapacity' smart constructor.
data ListProvisionedCapacity = ListProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either
    -- specify an AWS account ID or optionally a single \'-\' (hyphen), in
    -- which case Amazon S3 Glacier uses the AWS account ID associated with the
    -- credentials used to sign the request. If you use an account ID, don\'t
    -- include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listProvisionedCapacity_accountId' - The AWS account ID of the account that owns the vault. You can either
-- specify an AWS account ID or optionally a single \'-\' (hyphen), in
-- which case Amazon S3 Glacier uses the AWS account ID associated with the
-- credentials used to sign the request. If you use an account ID, don\'t
-- include any hyphens (\'-\') in the ID.
newListProvisionedCapacity ::
  -- | 'accountId'
  Prelude.Text ->
  ListProvisionedCapacity
newListProvisionedCapacity pAccountId_ =
  ListProvisionedCapacity' {accountId = pAccountId_}

-- | The AWS account ID of the account that owns the vault. You can either
-- specify an AWS account ID or optionally a single \'-\' (hyphen), in
-- which case Amazon S3 Glacier uses the AWS account ID associated with the
-- credentials used to sign the request. If you use an account ID, don\'t
-- include any hyphens (\'-\') in the ID.
listProvisionedCapacity_accountId :: Lens.Lens' ListProvisionedCapacity Prelude.Text
listProvisionedCapacity_accountId = Lens.lens (\ListProvisionedCapacity' {accountId} -> accountId) (\s@ListProvisionedCapacity' {} a -> s {accountId = a} :: ListProvisionedCapacity)

instance Core.AWSRequest ListProvisionedCapacity where
  type
    AWSResponse ListProvisionedCapacity =
      ListProvisionedCapacityResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedCapacityResponse'
            Prelude.<$> ( x Data..?> "ProvisionedCapacityList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProvisionedCapacity where
  hashWithSalt _salt ListProvisionedCapacity' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListProvisionedCapacity where
  rnf ListProvisionedCapacity' {..} =
    Prelude.rnf accountId

instance Data.ToHeaders ListProvisionedCapacity where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListProvisionedCapacity where
  toPath ListProvisionedCapacity' {..} =
    Prelude.mconcat
      ["/", Data.toBS accountId, "/provisioned-capacity"]

instance Data.ToQuery ListProvisionedCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProvisionedCapacityResponse' smart constructor.
data ListProvisionedCapacityResponse = ListProvisionedCapacityResponse'
  { -- | The response body contains the following JSON fields.
    provisionedCapacityList :: Prelude.Maybe [ProvisionedCapacityDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedCapacityList', 'listProvisionedCapacityResponse_provisionedCapacityList' - The response body contains the following JSON fields.
--
-- 'httpStatus', 'listProvisionedCapacityResponse_httpStatus' - The response's http status code.
newListProvisionedCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisionedCapacityResponse
newListProvisionedCapacityResponse pHttpStatus_ =
  ListProvisionedCapacityResponse'
    { provisionedCapacityList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response body contains the following JSON fields.
listProvisionedCapacityResponse_provisionedCapacityList :: Lens.Lens' ListProvisionedCapacityResponse (Prelude.Maybe [ProvisionedCapacityDescription])
listProvisionedCapacityResponse_provisionedCapacityList = Lens.lens (\ListProvisionedCapacityResponse' {provisionedCapacityList} -> provisionedCapacityList) (\s@ListProvisionedCapacityResponse' {} a -> s {provisionedCapacityList = a} :: ListProvisionedCapacityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProvisionedCapacityResponse_httpStatus :: Lens.Lens' ListProvisionedCapacityResponse Prelude.Int
listProvisionedCapacityResponse_httpStatus = Lens.lens (\ListProvisionedCapacityResponse' {httpStatus} -> httpStatus) (\s@ListProvisionedCapacityResponse' {} a -> s {httpStatus = a} :: ListProvisionedCapacityResponse)

instance
  Prelude.NFData
    ListProvisionedCapacityResponse
  where
  rnf ListProvisionedCapacityResponse' {..} =
    Prelude.rnf provisionedCapacityList
      `Prelude.seq` Prelude.rnf httpStatus
