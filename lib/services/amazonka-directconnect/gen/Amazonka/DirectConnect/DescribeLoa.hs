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
-- Module      : Amazonka.DirectConnect.DescribeLoa
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the LOA-CFA for a connection, interconnect, or link aggregation
-- group (LAG).
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA)
-- is a document that is used when establishing your cross connect to
-- Amazon Web Services at the colocation facility. For more information,
-- see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at Direct Connect Locations>
-- in the /Direct Connect User Guide/.
module Amazonka.DirectConnect.DescribeLoa
  ( -- * Creating a Request
    DescribeLoa (..),
    newDescribeLoa,

    -- * Request Lenses
    describeLoa_loaContentType,
    describeLoa_providerName,
    describeLoa_connectionId,

    -- * Destructuring the Response
    DescribeLoaResponse (..),
    newDescribeLoaResponse,

    -- * Response Lenses
    describeLoaResponse_loaContent,
    describeLoaResponse_loaContentType,
    describeLoaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLoa' smart constructor.
data DescribeLoa = DescribeLoa'
  { -- | The standard media type for the LOA-CFA document. The only supported
    -- value is application\/pdf.
    loaContentType :: Prelude.Maybe LoaContentType,
    -- | The name of the service provider who establishes connectivity on your
    -- behalf. If you specify this parameter, the LOA-CFA lists the provider
    -- name alongside your company name as the requester of the cross connect.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a connection, LAG, or interconnect.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoa' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loaContentType', 'describeLoa_loaContentType' - The standard media type for the LOA-CFA document. The only supported
-- value is application\/pdf.
--
-- 'providerName', 'describeLoa_providerName' - The name of the service provider who establishes connectivity on your
-- behalf. If you specify this parameter, the LOA-CFA lists the provider
-- name alongside your company name as the requester of the cross connect.
--
-- 'connectionId', 'describeLoa_connectionId' - The ID of a connection, LAG, or interconnect.
newDescribeLoa ::
  -- | 'connectionId'
  Prelude.Text ->
  DescribeLoa
newDescribeLoa pConnectionId_ =
  DescribeLoa'
    { loaContentType = Prelude.Nothing,
      providerName = Prelude.Nothing,
      connectionId = pConnectionId_
    }

-- | The standard media type for the LOA-CFA document. The only supported
-- value is application\/pdf.
describeLoa_loaContentType :: Lens.Lens' DescribeLoa (Prelude.Maybe LoaContentType)
describeLoa_loaContentType = Lens.lens (\DescribeLoa' {loaContentType} -> loaContentType) (\s@DescribeLoa' {} a -> s {loaContentType = a} :: DescribeLoa)

-- | The name of the service provider who establishes connectivity on your
-- behalf. If you specify this parameter, the LOA-CFA lists the provider
-- name alongside your company name as the requester of the cross connect.
describeLoa_providerName :: Lens.Lens' DescribeLoa (Prelude.Maybe Prelude.Text)
describeLoa_providerName = Lens.lens (\DescribeLoa' {providerName} -> providerName) (\s@DescribeLoa' {} a -> s {providerName = a} :: DescribeLoa)

-- | The ID of a connection, LAG, or interconnect.
describeLoa_connectionId :: Lens.Lens' DescribeLoa Prelude.Text
describeLoa_connectionId = Lens.lens (\DescribeLoa' {connectionId} -> connectionId) (\s@DescribeLoa' {} a -> s {connectionId = a} :: DescribeLoa)

instance Core.AWSRequest DescribeLoa where
  type AWSResponse DescribeLoa = DescribeLoaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoaResponse'
            Prelude.<$> (x Data..?> "loaContent")
            Prelude.<*> (x Data..?> "loaContentType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLoa where
  hashWithSalt _salt DescribeLoa' {..} =
    _salt
      `Prelude.hashWithSalt` loaContentType
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DescribeLoa where
  rnf DescribeLoa' {..} =
    Prelude.rnf loaContentType
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf connectionId

instance Data.ToHeaders DescribeLoa where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeLoa" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLoa where
  toJSON DescribeLoa' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("loaContentType" Data..=)
              Prelude.<$> loaContentType,
            ("providerName" Data..=) Prelude.<$> providerName,
            Prelude.Just ("connectionId" Data..= connectionId)
          ]
      )

instance Data.ToPath DescribeLoa where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLoa where
  toQuery = Prelude.const Prelude.mempty

-- | Information about a Letter of Authorization - Connecting Facility
-- Assignment (LOA-CFA) for a connection.
--
-- /See:/ 'newDescribeLoaResponse' smart constructor.
data DescribeLoaResponse = DescribeLoaResponse'
  { -- | The binary contents of the LOA-CFA document.
    loaContent :: Prelude.Maybe Data.Base64,
    -- | The standard media type for the LOA-CFA document. The only supported
    -- value is application\/pdf.
    loaContentType :: Prelude.Maybe LoaContentType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loaContent', 'describeLoaResponse_loaContent' - The binary contents of the LOA-CFA document.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'loaContentType', 'describeLoaResponse_loaContentType' - The standard media type for the LOA-CFA document. The only supported
-- value is application\/pdf.
--
-- 'httpStatus', 'describeLoaResponse_httpStatus' - The response's http status code.
newDescribeLoaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoaResponse
newDescribeLoaResponse pHttpStatus_ =
  DescribeLoaResponse'
    { loaContent = Prelude.Nothing,
      loaContentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The binary contents of the LOA-CFA document.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
describeLoaResponse_loaContent :: Lens.Lens' DescribeLoaResponse (Prelude.Maybe Prelude.ByteString)
describeLoaResponse_loaContent = Lens.lens (\DescribeLoaResponse' {loaContent} -> loaContent) (\s@DescribeLoaResponse' {} a -> s {loaContent = a} :: DescribeLoaResponse) Prelude.. Lens.mapping Data._Base64

-- | The standard media type for the LOA-CFA document. The only supported
-- value is application\/pdf.
describeLoaResponse_loaContentType :: Lens.Lens' DescribeLoaResponse (Prelude.Maybe LoaContentType)
describeLoaResponse_loaContentType = Lens.lens (\DescribeLoaResponse' {loaContentType} -> loaContentType) (\s@DescribeLoaResponse' {} a -> s {loaContentType = a} :: DescribeLoaResponse)

-- | The response's http status code.
describeLoaResponse_httpStatus :: Lens.Lens' DescribeLoaResponse Prelude.Int
describeLoaResponse_httpStatus = Lens.lens (\DescribeLoaResponse' {httpStatus} -> httpStatus) (\s@DescribeLoaResponse' {} a -> s {httpStatus = a} :: DescribeLoaResponse)

instance Prelude.NFData DescribeLoaResponse where
  rnf DescribeLoaResponse' {..} =
    Prelude.rnf loaContent
      `Prelude.seq` Prelude.rnf loaContentType
      `Prelude.seq` Prelude.rnf httpStatus
