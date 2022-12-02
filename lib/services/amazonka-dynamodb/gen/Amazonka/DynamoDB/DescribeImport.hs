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
-- Module      : Amazonka.DynamoDB.DescribeImport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the properties of the import.
module Amazonka.DynamoDB.DescribeImport
  ( -- * Creating a Request
    DescribeImport (..),
    newDescribeImport,

    -- * Request Lenses
    describeImport_importArn,

    -- * Destructuring the Response
    DescribeImportResponse (..),
    newDescribeImportResponse,

    -- * Response Lenses
    describeImportResponse_httpStatus,
    describeImportResponse_importTableDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImport' smart constructor.
data DescribeImport = DescribeImport'
  { -- | The Amazon Resource Name (ARN) associated with the table you\'re
    -- importing to.
    importArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importArn', 'describeImport_importArn' - The Amazon Resource Name (ARN) associated with the table you\'re
-- importing to.
newDescribeImport ::
  -- | 'importArn'
  Prelude.Text ->
  DescribeImport
newDescribeImport pImportArn_ =
  DescribeImport' {importArn = pImportArn_}

-- | The Amazon Resource Name (ARN) associated with the table you\'re
-- importing to.
describeImport_importArn :: Lens.Lens' DescribeImport Prelude.Text
describeImport_importArn = Lens.lens (\DescribeImport' {importArn} -> importArn) (\s@DescribeImport' {} a -> s {importArn = a} :: DescribeImport)

instance Core.AWSRequest DescribeImport where
  type
    AWSResponse DescribeImport =
      DescribeImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ImportTableDescription")
      )

instance Prelude.Hashable DescribeImport where
  hashWithSalt _salt DescribeImport' {..} =
    _salt `Prelude.hashWithSalt` importArn

instance Prelude.NFData DescribeImport where
  rnf DescribeImport' {..} = Prelude.rnf importArn

instance Data.ToHeaders DescribeImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImport where
  toJSON DescribeImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImportArn" Data..= importArn)]
      )

instance Data.ToPath DescribeImport where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImportResponse' smart constructor.
data DescribeImportResponse = DescribeImportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the properties of the table created for the import, and
    -- parameters of the import. The import parameters include import status,
    -- how many items were processed, and how many errors were encountered.
    importTableDescription :: ImportTableDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeImportResponse_httpStatus' - The response's http status code.
--
-- 'importTableDescription', 'describeImportResponse_importTableDescription' - Represents the properties of the table created for the import, and
-- parameters of the import. The import parameters include import status,
-- how many items were processed, and how many errors were encountered.
newDescribeImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importTableDescription'
  ImportTableDescription ->
  DescribeImportResponse
newDescribeImportResponse
  pHttpStatus_
  pImportTableDescription_ =
    DescribeImportResponse'
      { httpStatus = pHttpStatus_,
        importTableDescription = pImportTableDescription_
      }

-- | The response's http status code.
describeImportResponse_httpStatus :: Lens.Lens' DescribeImportResponse Prelude.Int
describeImportResponse_httpStatus = Lens.lens (\DescribeImportResponse' {httpStatus} -> httpStatus) (\s@DescribeImportResponse' {} a -> s {httpStatus = a} :: DescribeImportResponse)

-- | Represents the properties of the table created for the import, and
-- parameters of the import. The import parameters include import status,
-- how many items were processed, and how many errors were encountered.
describeImportResponse_importTableDescription :: Lens.Lens' DescribeImportResponse ImportTableDescription
describeImportResponse_importTableDescription = Lens.lens (\DescribeImportResponse' {importTableDescription} -> importTableDescription) (\s@DescribeImportResponse' {} a -> s {importTableDescription = a} :: DescribeImportResponse)

instance Prelude.NFData DescribeImportResponse where
  rnf DescribeImportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importTableDescription
