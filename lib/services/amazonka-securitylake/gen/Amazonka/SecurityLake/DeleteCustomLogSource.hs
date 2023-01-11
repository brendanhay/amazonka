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
-- Removes a custom log source from Amazon Security Lake.
module Amazonka.SecurityLake.DeleteCustomLogSource
  ( -- * Creating a Request
    DeleteCustomLogSource (..),
    newDeleteCustomLogSource,

    -- * Request Lenses
    deleteCustomLogSource_customSourceName,

    -- * Destructuring the Response
    DeleteCustomLogSourceResponse (..),
    newDeleteCustomLogSourceResponse,

    -- * Response Lenses
    deleteCustomLogSourceResponse_httpStatus,
    deleteCustomLogSourceResponse_customDataLocation,
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
  { -- | The custom source name for the custom log source.
    customSourceName :: Prelude.Text
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
-- 'customSourceName', 'deleteCustomLogSource_customSourceName' - The custom source name for the custom log source.
newDeleteCustomLogSource ::
  -- | 'customSourceName'
  Prelude.Text ->
  DeleteCustomLogSource
newDeleteCustomLogSource pCustomSourceName_ =
  DeleteCustomLogSource'
    { customSourceName =
        pCustomSourceName_
    }

-- | The custom source name for the custom log source.
deleteCustomLogSource_customSourceName :: Lens.Lens' DeleteCustomLogSource Prelude.Text
deleteCustomLogSource_customSourceName = Lens.lens (\DeleteCustomLogSource' {customSourceName} -> customSourceName) (\s@DeleteCustomLogSource' {} a -> s {customSourceName = a} :: DeleteCustomLogSource)

instance Core.AWSRequest DeleteCustomLogSource where
  type
    AWSResponse DeleteCustomLogSource =
      DeleteCustomLogSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCustomLogSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "customDataLocation")
      )

instance Prelude.Hashable DeleteCustomLogSource where
  hashWithSalt _salt DeleteCustomLogSource' {..} =
    _salt `Prelude.hashWithSalt` customSourceName

instance Prelude.NFData DeleteCustomLogSource where
  rnf DeleteCustomLogSource' {..} =
    Prelude.rnf customSourceName

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
  toPath = Prelude.const "/v1/logsources/custom"

instance Data.ToQuery DeleteCustomLogSource where
  toQuery DeleteCustomLogSource' {..} =
    Prelude.mconcat
      ["customSourceName" Data.=: customSourceName]

-- | /See:/ 'newDeleteCustomLogSourceResponse' smart constructor.
data DeleteCustomLogSourceResponse = DeleteCustomLogSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The location of the partition in the Amazon S3 bucket for Security Lake.
    customDataLocation :: Prelude.Text
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
--
-- 'customDataLocation', 'deleteCustomLogSourceResponse_customDataLocation' - The location of the partition in the Amazon S3 bucket for Security Lake.
newDeleteCustomLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'customDataLocation'
  Prelude.Text ->
  DeleteCustomLogSourceResponse
newDeleteCustomLogSourceResponse
  pHttpStatus_
  pCustomDataLocation_ =
    DeleteCustomLogSourceResponse'
      { httpStatus =
          pHttpStatus_,
        customDataLocation = pCustomDataLocation_
      }

-- | The response's http status code.
deleteCustomLogSourceResponse_httpStatus :: Lens.Lens' DeleteCustomLogSourceResponse Prelude.Int
deleteCustomLogSourceResponse_httpStatus = Lens.lens (\DeleteCustomLogSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomLogSourceResponse' {} a -> s {httpStatus = a} :: DeleteCustomLogSourceResponse)

-- | The location of the partition in the Amazon S3 bucket for Security Lake.
deleteCustomLogSourceResponse_customDataLocation :: Lens.Lens' DeleteCustomLogSourceResponse Prelude.Text
deleteCustomLogSourceResponse_customDataLocation = Lens.lens (\DeleteCustomLogSourceResponse' {customDataLocation} -> customDataLocation) (\s@DeleteCustomLogSourceResponse' {} a -> s {customDataLocation = a} :: DeleteCustomLogSourceResponse)

instance Prelude.NFData DeleteCustomLogSourceResponse where
  rnf DeleteCustomLogSourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf customDataLocation
