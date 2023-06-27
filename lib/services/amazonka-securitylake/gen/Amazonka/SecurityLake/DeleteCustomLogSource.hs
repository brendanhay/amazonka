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
-- Module      : Amazonka.SecurityLake.DeleteCustomLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a custom log source from Amazon Security Lake, to stop sending
-- data from the custom source to Security Lake.
module Amazonka.SecurityLake.DeleteCustomLogSource
  ( -- * Creating a Request
    DeleteCustomLogSource (..),
    newDeleteCustomLogSource,

    -- * Request Lenses
    deleteCustomLogSource_sourceVersion,
    deleteCustomLogSource_sourceName,

    -- * Destructuring the Response
    DeleteCustomLogSourceResponse (..),
    newDeleteCustomLogSourceResponse,

    -- * Response Lenses
    deleteCustomLogSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteCustomLogSource' smart constructor.
data DeleteCustomLogSource = DeleteCustomLogSource'
  { -- | The source version for the third-party custom source. You can limit the
    -- custom source removal to the specified source version.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | The source name of custom log source that you want to delete.
    sourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'deleteCustomLogSource_sourceVersion' - The source version for the third-party custom source. You can limit the
-- custom source removal to the specified source version.
--
-- 'sourceName', 'deleteCustomLogSource_sourceName' - The source name of custom log source that you want to delete.
newDeleteCustomLogSource ::
  -- | 'sourceName'
  Prelude.Text ->
  DeleteCustomLogSource
newDeleteCustomLogSource pSourceName_ =
  DeleteCustomLogSource'
    { sourceVersion =
        Prelude.Nothing,
      sourceName = pSourceName_
    }

-- | The source version for the third-party custom source. You can limit the
-- custom source removal to the specified source version.
deleteCustomLogSource_sourceVersion :: Lens.Lens' DeleteCustomLogSource (Prelude.Maybe Prelude.Text)
deleteCustomLogSource_sourceVersion = Lens.lens (\DeleteCustomLogSource' {sourceVersion} -> sourceVersion) (\s@DeleteCustomLogSource' {} a -> s {sourceVersion = a} :: DeleteCustomLogSource)

-- | The source name of custom log source that you want to delete.
deleteCustomLogSource_sourceName :: Lens.Lens' DeleteCustomLogSource Prelude.Text
deleteCustomLogSource_sourceName = Lens.lens (\DeleteCustomLogSource' {sourceName} -> sourceName) (\s@DeleteCustomLogSource' {} a -> s {sourceName = a} :: DeleteCustomLogSource)

instance Core.AWSRequest DeleteCustomLogSource where
  type
    AWSResponse DeleteCustomLogSource =
      DeleteCustomLogSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomLogSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomLogSource where
  hashWithSalt _salt DeleteCustomLogSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData DeleteCustomLogSource where
  rnf DeleteCustomLogSource' {..} =
    Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf sourceName

instance Data.ToHeaders DeleteCustomLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCustomLogSource where
  toPath DeleteCustomLogSource' {..} =
    Prelude.mconcat
      [ "/v1/datalake/logsources/custom/",
        Data.toBS sourceName
      ]

instance Data.ToQuery DeleteCustomLogSource where
  toQuery DeleteCustomLogSource' {..} =
    Prelude.mconcat
      ["sourceVersion" Data.=: sourceVersion]

-- | /See:/ 'newDeleteCustomLogSourceResponse' smart constructor.
data DeleteCustomLogSourceResponse = DeleteCustomLogSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomLogSourceResponse_httpStatus' - The response's http status code.
newDeleteCustomLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomLogSourceResponse
newDeleteCustomLogSourceResponse pHttpStatus_ =
  DeleteCustomLogSourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomLogSourceResponse_httpStatus :: Lens.Lens' DeleteCustomLogSourceResponse Prelude.Int
deleteCustomLogSourceResponse_httpStatus = Lens.lens (\DeleteCustomLogSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomLogSourceResponse' {} a -> s {httpStatus = a} :: DeleteCustomLogSourceResponse)

instance Prelude.NFData DeleteCustomLogSourceResponse where
  rnf DeleteCustomLogSourceResponse' {..} =
    Prelude.rnf httpStatus
