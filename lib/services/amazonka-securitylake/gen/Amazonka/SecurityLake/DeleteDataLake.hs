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
-- Module      : Amazonka.SecurityLake.DeleteDataLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you disable Amazon Security Lake from your account, Security Lake
-- is disabled in all Amazon Web Services Regions and it stops collecting
-- data from your sources. Also, this API automatically takes steps to
-- remove the account from Security Lake. However, Security Lake retains
-- all of your existing settings and the resources that it created in your
-- Amazon Web Services account in the current Amazon Web Services Region.
--
-- The @DeleteDataLake@ operation does not delete the data that is stored
-- in your Amazon S3 bucket, which is owned by your Amazon Web Services
-- account. For more information, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/disable-security-lake.html Amazon Security Lake User Guide>.
module Amazonka.SecurityLake.DeleteDataLake
  ( -- * Creating a Request
    DeleteDataLake (..),
    newDeleteDataLake,

    -- * Request Lenses
    deleteDataLake_regions,

    -- * Destructuring the Response
    DeleteDataLakeResponse (..),
    newDeleteDataLakeResponse,

    -- * Response Lenses
    deleteDataLakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDataLake' smart constructor.
data DeleteDataLake = DeleteDataLake'
  { -- | The list of Regions where Security Lake is enabled.
    regions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'deleteDataLake_regions' - The list of Regions where Security Lake is enabled.
newDeleteDataLake ::
  DeleteDataLake
newDeleteDataLake =
  DeleteDataLake' {regions = Prelude.mempty}

-- | The list of Regions where Security Lake is enabled.
deleteDataLake_regions :: Lens.Lens' DeleteDataLake [Prelude.Text]
deleteDataLake_regions = Lens.lens (\DeleteDataLake' {regions} -> regions) (\s@DeleteDataLake' {} a -> s {regions = a} :: DeleteDataLake) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteDataLake where
  type
    AWSResponse DeleteDataLake =
      DeleteDataLakeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataLakeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataLake where
  hashWithSalt _salt DeleteDataLake' {..} =
    _salt `Prelude.hashWithSalt` regions

instance Prelude.NFData DeleteDataLake where
  rnf DeleteDataLake' {..} = Prelude.rnf regions

instance Data.ToHeaders DeleteDataLake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataLake where
  toJSON DeleteDataLake' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("regions" Data..= regions)]
      )

instance Data.ToPath DeleteDataLake where
  toPath = Prelude.const "/v1/datalake/delete"

instance Data.ToQuery DeleteDataLake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataLakeResponse' smart constructor.
data DeleteDataLakeResponse = DeleteDataLakeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataLakeResponse_httpStatus' - The response's http status code.
newDeleteDataLakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataLakeResponse
newDeleteDataLakeResponse pHttpStatus_ =
  DeleteDataLakeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDataLakeResponse_httpStatus :: Lens.Lens' DeleteDataLakeResponse Prelude.Int
deleteDataLakeResponse_httpStatus = Lens.lens (\DeleteDataLakeResponse' {httpStatus} -> httpStatus) (\s@DeleteDataLakeResponse' {} a -> s {httpStatus = a} :: DeleteDataLakeResponse)

instance Prelude.NFData DeleteDataLakeResponse where
  rnf DeleteDataLakeResponse' {..} =
    Prelude.rnf httpStatus
