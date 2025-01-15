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
-- Module      : Amazonka.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
module Amazonka.ServiceCatalog.DescribeCopyProductStatus
  ( -- * Creating a Request
    DescribeCopyProductStatus (..),
    newDescribeCopyProductStatus,

    -- * Request Lenses
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,

    -- * Destructuring the Response
    DescribeCopyProductStatusResponse (..),
    newDescribeCopyProductStatusResponse,

    -- * Response Lenses
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The token for the copy product operation. This token is returned by
    -- CopyProduct.
    copyProductToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCopyProductStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeCopyProductStatus_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'copyProductToken', 'describeCopyProductStatus_copyProductToken' - The token for the copy product operation. This token is returned by
-- CopyProduct.
newDescribeCopyProductStatus ::
  -- | 'copyProductToken'
  Prelude.Text ->
  DescribeCopyProductStatus
newDescribeCopyProductStatus pCopyProductToken_ =
  DescribeCopyProductStatus'
    { acceptLanguage =
        Prelude.Nothing,
      copyProductToken = pCopyProductToken_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeCopyProductStatus_acceptLanguage :: Lens.Lens' DescribeCopyProductStatus (Prelude.Maybe Prelude.Text)
describeCopyProductStatus_acceptLanguage = Lens.lens (\DescribeCopyProductStatus' {acceptLanguage} -> acceptLanguage) (\s@DescribeCopyProductStatus' {} a -> s {acceptLanguage = a} :: DescribeCopyProductStatus)

-- | The token for the copy product operation. This token is returned by
-- CopyProduct.
describeCopyProductStatus_copyProductToken :: Lens.Lens' DescribeCopyProductStatus Prelude.Text
describeCopyProductStatus_copyProductToken = Lens.lens (\DescribeCopyProductStatus' {copyProductToken} -> copyProductToken) (\s@DescribeCopyProductStatus' {} a -> s {copyProductToken = a} :: DescribeCopyProductStatus)

instance Core.AWSRequest DescribeCopyProductStatus where
  type
    AWSResponse DescribeCopyProductStatus =
      DescribeCopyProductStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCopyProductStatusResponse'
            Prelude.<$> (x Data..?> "CopyProductStatus")
            Prelude.<*> (x Data..?> "StatusDetail")
            Prelude.<*> (x Data..?> "TargetProductId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCopyProductStatus where
  hashWithSalt _salt DescribeCopyProductStatus' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` copyProductToken

instance Prelude.NFData DescribeCopyProductStatus where
  rnf DescribeCopyProductStatus' {..} =
    Prelude.rnf acceptLanguage `Prelude.seq`
      Prelude.rnf copyProductToken

instance Data.ToHeaders DescribeCopyProductStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeCopyProductStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCopyProductStatus where
  toJSON DescribeCopyProductStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ("CopyProductToken" Data..= copyProductToken)
          ]
      )

instance Data.ToPath DescribeCopyProductStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCopyProductStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { -- | The status of the copy product operation.
    copyProductStatus :: Prelude.Maybe CopyProductStatus,
    -- | The status message.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the copied product.
    targetProductId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCopyProductStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyProductStatus', 'describeCopyProductStatusResponse_copyProductStatus' - The status of the copy product operation.
--
-- 'statusDetail', 'describeCopyProductStatusResponse_statusDetail' - The status message.
--
-- 'targetProductId', 'describeCopyProductStatusResponse_targetProductId' - The identifier of the copied product.
--
-- 'httpStatus', 'describeCopyProductStatusResponse_httpStatus' - The response's http status code.
newDescribeCopyProductStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCopyProductStatusResponse
newDescribeCopyProductStatusResponse pHttpStatus_ =
  DescribeCopyProductStatusResponse'
    { copyProductStatus =
        Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      targetProductId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the copy product operation.
describeCopyProductStatusResponse_copyProductStatus :: Lens.Lens' DescribeCopyProductStatusResponse (Prelude.Maybe CopyProductStatus)
describeCopyProductStatusResponse_copyProductStatus = Lens.lens (\DescribeCopyProductStatusResponse' {copyProductStatus} -> copyProductStatus) (\s@DescribeCopyProductStatusResponse' {} a -> s {copyProductStatus = a} :: DescribeCopyProductStatusResponse)

-- | The status message.
describeCopyProductStatusResponse_statusDetail :: Lens.Lens' DescribeCopyProductStatusResponse (Prelude.Maybe Prelude.Text)
describeCopyProductStatusResponse_statusDetail = Lens.lens (\DescribeCopyProductStatusResponse' {statusDetail} -> statusDetail) (\s@DescribeCopyProductStatusResponse' {} a -> s {statusDetail = a} :: DescribeCopyProductStatusResponse)

-- | The identifier of the copied product.
describeCopyProductStatusResponse_targetProductId :: Lens.Lens' DescribeCopyProductStatusResponse (Prelude.Maybe Prelude.Text)
describeCopyProductStatusResponse_targetProductId = Lens.lens (\DescribeCopyProductStatusResponse' {targetProductId} -> targetProductId) (\s@DescribeCopyProductStatusResponse' {} a -> s {targetProductId = a} :: DescribeCopyProductStatusResponse)

-- | The response's http status code.
describeCopyProductStatusResponse_httpStatus :: Lens.Lens' DescribeCopyProductStatusResponse Prelude.Int
describeCopyProductStatusResponse_httpStatus = Lens.lens (\DescribeCopyProductStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeCopyProductStatusResponse' {} a -> s {httpStatus = a} :: DescribeCopyProductStatusResponse)

instance
  Prelude.NFData
    DescribeCopyProductStatusResponse
  where
  rnf DescribeCopyProductStatusResponse' {..} =
    Prelude.rnf copyProductStatus `Prelude.seq`
      Prelude.rnf statusDetail `Prelude.seq`
        Prelude.rnf targetProductId `Prelude.seq`
          Prelude.rnf httpStatus
