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
-- Module      : Amazonka.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool
-- to the configured stream. Customers are limited to one successful bulk
-- publish per 24 hours. Bulk publish is an asynchronous request, customers
-- can see the status of the request via the GetBulkPublishDetails
-- operation.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Amazonka.CognitoSync.BulkPublish
  ( -- * Creating a Request
    BulkPublish (..),
    newBulkPublish,

    -- * Request Lenses
    bulkPublish_identityPoolId,

    -- * Destructuring the Response
    BulkPublishResponse (..),
    newBulkPublishResponse,

    -- * Response Lenses
    bulkPublishResponse_identityPoolId,
    bulkPublishResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the BulkPublish operation.
--
-- /See:/ 'newBulkPublish' smart constructor.
data BulkPublish = BulkPublish'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkPublish' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'bulkPublish_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
newBulkPublish ::
  -- | 'identityPoolId'
  Prelude.Text ->
  BulkPublish
newBulkPublish pIdentityPoolId_ =
  BulkPublish' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bulkPublish_identityPoolId :: Lens.Lens' BulkPublish Prelude.Text
bulkPublish_identityPoolId = Lens.lens (\BulkPublish' {identityPoolId} -> identityPoolId) (\s@BulkPublish' {} a -> s {identityPoolId = a} :: BulkPublish)

instance Core.AWSRequest BulkPublish where
  type AWSResponse BulkPublish = BulkPublishResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BulkPublishResponse'
            Prelude.<$> (x Data..?> "IdentityPoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BulkPublish where
  hashWithSalt _salt BulkPublish' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData BulkPublish where
  rnf BulkPublish' {..} = Prelude.rnf identityPoolId

instance Data.ToHeaders BulkPublish where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BulkPublish where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath BulkPublish where
  toPath BulkPublish' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/bulkpublish"
      ]

instance Data.ToQuery BulkPublish where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the BulkPublish operation.
--
-- /See:/ 'newBulkPublishResponse' smart constructor.
data BulkPublishResponse = BulkPublishResponse'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkPublishResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'bulkPublishResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'httpStatus', 'bulkPublishResponse_httpStatus' - The response's http status code.
newBulkPublishResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BulkPublishResponse
newBulkPublishResponse pHttpStatus_ =
  BulkPublishResponse'
    { identityPoolId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
bulkPublishResponse_identityPoolId :: Lens.Lens' BulkPublishResponse (Prelude.Maybe Prelude.Text)
bulkPublishResponse_identityPoolId = Lens.lens (\BulkPublishResponse' {identityPoolId} -> identityPoolId) (\s@BulkPublishResponse' {} a -> s {identityPoolId = a} :: BulkPublishResponse)

-- | The response's http status code.
bulkPublishResponse_httpStatus :: Lens.Lens' BulkPublishResponse Prelude.Int
bulkPublishResponse_httpStatus = Lens.lens (\BulkPublishResponse' {httpStatus} -> httpStatus) (\s@BulkPublishResponse' {} a -> s {httpStatus = a} :: BulkPublishResponse)

instance Prelude.NFData BulkPublishResponse where
  rnf BulkPublishResponse' {..} =
    Prelude.rnf identityPoolId `Prelude.seq`
      Prelude.rnf httpStatus
