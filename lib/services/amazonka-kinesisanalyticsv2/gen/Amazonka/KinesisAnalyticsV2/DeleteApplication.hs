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
-- Module      : Amazonka.KinesisAnalyticsV2.DeleteApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Kinesis Data Analytics halts
-- application execution and deletes the application.
module Amazonka.KinesisAnalyticsV2.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationName,
    deleteApplication_createTimestamp,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,

    -- * Response Lenses
    deleteApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The name of the application to delete.
    applicationName :: Prelude.Text,
    -- | Use the @DescribeApplication@ operation to get this value.
    createTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplication_applicationName' - The name of the application to delete.
--
-- 'createTimestamp', 'deleteApplication_createTimestamp' - Use the @DescribeApplication@ operation to get this value.
newDeleteApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'createTimestamp'
  Prelude.UTCTime ->
  DeleteApplication
newDeleteApplication
  pApplicationName_
  pCreateTimestamp_ =
    DeleteApplication'
      { applicationName =
          pApplicationName_,
        createTimestamp =
          Data._Time Lens.# pCreateTimestamp_
      }

-- | The name of the application to delete.
deleteApplication_applicationName :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationName = Lens.lens (\DeleteApplication' {applicationName} -> applicationName) (\s@DeleteApplication' {} a -> s {applicationName = a} :: DeleteApplication)

-- | Use the @DescribeApplication@ operation to get this value.
deleteApplication_createTimestamp :: Lens.Lens' DeleteApplication Prelude.UTCTime
deleteApplication_createTimestamp = Lens.lens (\DeleteApplication' {createTimestamp} -> createTimestamp) (\s@DeleteApplication' {} a -> s {createTimestamp = a} :: DeleteApplication) Prelude.. Data._Time

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` createTimestamp

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf createTimestamp

instance Data.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.DeleteApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ("CreateTimestamp" Data..= createTimestamp)
          ]
      )

instance Data.ToPath DeleteApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationResponse_httpStatus' - The response's http status code.
newDeleteApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationResponse
newDeleteApplicationResponse pHttpStatus_ =
  DeleteApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationResponse_httpStatus :: Lens.Lens' DeleteApplicationResponse Prelude.Int
deleteApplicationResponse_httpStatus = Lens.lens (\DeleteApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationResponse)

instance Prelude.NFData DeleteApplicationResponse where
  rnf DeleteApplicationResponse' {..} =
    Prelude.rnf httpStatus
