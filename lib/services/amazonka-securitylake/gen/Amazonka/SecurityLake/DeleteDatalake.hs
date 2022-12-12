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
-- Module      : Amazonka.SecurityLake.DeleteDatalake
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you delete Amazon Security Lake from your account, Security Lake is
-- disabled in all Regions. Also, this API automatically performs the
-- off-boarding steps to off-board the account from Security Lake . This
-- includes ingesting security data from sources, storing data, and making
-- data accessible to subscribers. Security Lake also deletes all the
-- existing settings and resources that it stores or maintains for your
-- account in the current Region, including security log and event data.
-- @DeleteDatalake@ does not delete the S3 bucket which is owned by the
-- Amazon Web Services account. For more information, see the Amazon
-- Security Lake User Guide.
module Amazonka.SecurityLake.DeleteDatalake
  ( -- * Creating a Request
    DeleteDatalake (..),
    newDeleteDatalake,

    -- * Destructuring the Response
    DeleteDatalakeResponse (..),
    newDeleteDatalakeResponse,

    -- * Response Lenses
    deleteDatalakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDatalake' smart constructor.
data DeleteDatalake = DeleteDatalake'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatalake ::
  DeleteDatalake
newDeleteDatalake = DeleteDatalake'

instance Core.AWSRequest DeleteDatalake where
  type
    AWSResponse DeleteDatalake =
      DeleteDatalakeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDatalakeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDatalake where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteDatalake where
  rnf _ = ()

instance Data.ToHeaders DeleteDatalake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDatalake where
  toPath = Prelude.const "/v1/datalake"

instance Data.ToQuery DeleteDatalake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatalakeResponse' smart constructor.
data DeleteDatalakeResponse = DeleteDatalakeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDatalakeResponse_httpStatus' - The response's http status code.
newDeleteDatalakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatalakeResponse
newDeleteDatalakeResponse pHttpStatus_ =
  DeleteDatalakeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDatalakeResponse_httpStatus :: Lens.Lens' DeleteDatalakeResponse Prelude.Int
deleteDatalakeResponse_httpStatus = Lens.lens (\DeleteDatalakeResponse' {httpStatus} -> httpStatus) (\s@DeleteDatalakeResponse' {} a -> s {httpStatus = a} :: DeleteDatalakeResponse)

instance Prelude.NFData DeleteDatalakeResponse where
  rnf DeleteDatalakeResponse' {..} =
    Prelude.rnf httpStatus
