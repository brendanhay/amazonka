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
-- Module      : Amazonka.SecurityLake.DeleteAwsLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a natively supported Amazon Web Service as an Amazon Security
-- Lake source. You can remove a source for one or more Regions. When you
-- remove the source, Security Lake stops collecting data from that source
-- in the specified Regions and accounts, and subscribers can no longer
-- consume new data from the source. However, subscribers can still consume
-- data that Security Lake collected from the source before removal.
--
-- You can choose any source type in any Amazon Web Services Region for
-- either accounts that are part of a trusted organization or standalone
-- accounts.
module Amazonka.SecurityLake.DeleteAwsLogSource
  ( -- * Creating a Request
    DeleteAwsLogSource (..),
    newDeleteAwsLogSource,

    -- * Request Lenses
    deleteAwsLogSource_sources,

    -- * Destructuring the Response
    DeleteAwsLogSourceResponse (..),
    newDeleteAwsLogSourceResponse,

    -- * Response Lenses
    deleteAwsLogSourceResponse_failed,
    deleteAwsLogSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteAwsLogSource' smart constructor.
data DeleteAwsLogSource = DeleteAwsLogSource'
  { -- | Specify the natively-supported Amazon Web Services service to remove as
    -- a source in Security Lake.
    sources :: [AwsLogSourceConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAwsLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'deleteAwsLogSource_sources' - Specify the natively-supported Amazon Web Services service to remove as
-- a source in Security Lake.
newDeleteAwsLogSource ::
  DeleteAwsLogSource
newDeleteAwsLogSource =
  DeleteAwsLogSource' {sources = Prelude.mempty}

-- | Specify the natively-supported Amazon Web Services service to remove as
-- a source in Security Lake.
deleteAwsLogSource_sources :: Lens.Lens' DeleteAwsLogSource [AwsLogSourceConfiguration]
deleteAwsLogSource_sources = Lens.lens (\DeleteAwsLogSource' {sources} -> sources) (\s@DeleteAwsLogSource' {} a -> s {sources = a} :: DeleteAwsLogSource) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteAwsLogSource where
  type
    AWSResponse DeleteAwsLogSource =
      DeleteAwsLogSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAwsLogSourceResponse'
            Prelude.<$> (x Data..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAwsLogSource where
  hashWithSalt _salt DeleteAwsLogSource' {..} =
    _salt `Prelude.hashWithSalt` sources

instance Prelude.NFData DeleteAwsLogSource where
  rnf DeleteAwsLogSource' {..} = Prelude.rnf sources

instance Data.ToHeaders DeleteAwsLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAwsLogSource where
  toJSON DeleteAwsLogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("sources" Data..= sources)]
      )

instance Data.ToPath DeleteAwsLogSource where
  toPath =
    Prelude.const "/v1/datalake/logsources/aws/delete"

instance Data.ToQuery DeleteAwsLogSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAwsLogSourceResponse' smart constructor.
data DeleteAwsLogSourceResponse = DeleteAwsLogSourceResponse'
  { -- | Deletion of the Amazon Web Services sources failed as the account is not
    -- a part of the organization.
    failed :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAwsLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'deleteAwsLogSourceResponse_failed' - Deletion of the Amazon Web Services sources failed as the account is not
-- a part of the organization.
--
-- 'httpStatus', 'deleteAwsLogSourceResponse_httpStatus' - The response's http status code.
newDeleteAwsLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAwsLogSourceResponse
newDeleteAwsLogSourceResponse pHttpStatus_ =
  DeleteAwsLogSourceResponse'
    { failed =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Deletion of the Amazon Web Services sources failed as the account is not
-- a part of the organization.
deleteAwsLogSourceResponse_failed :: Lens.Lens' DeleteAwsLogSourceResponse (Prelude.Maybe [Prelude.Text])
deleteAwsLogSourceResponse_failed = Lens.lens (\DeleteAwsLogSourceResponse' {failed} -> failed) (\s@DeleteAwsLogSourceResponse' {} a -> s {failed = a} :: DeleteAwsLogSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteAwsLogSourceResponse_httpStatus :: Lens.Lens' DeleteAwsLogSourceResponse Prelude.Int
deleteAwsLogSourceResponse_httpStatus = Lens.lens (\DeleteAwsLogSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteAwsLogSourceResponse' {} a -> s {httpStatus = a} :: DeleteAwsLogSourceResponse)

instance Prelude.NFData DeleteAwsLogSourceResponse where
  rnf DeleteAwsLogSourceResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf httpStatus
