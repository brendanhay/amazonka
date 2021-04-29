{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Deletes the specified application. Amazon Kinesis Analytics halts
-- application execution and deletes the application, including any
-- application artifacts (such as in-application streams, reference table,
-- and application code).
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:DeleteApplication@ action.
module Network.AWS.KinesisAnalytics.DeleteApplication
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

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | Name of the Amazon Kinesis Analytics application to delete.
    applicationName :: Prelude.Text,
    -- | You can use the @DescribeApplication@ operation to get this value.
    createTimestamp :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplication_applicationName' - Name of the Amazon Kinesis Analytics application to delete.
--
-- 'createTimestamp', 'deleteApplication_createTimestamp' - You can use the @DescribeApplication@ operation to get this value.
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
          Prelude._Time Lens.# pCreateTimestamp_
      }

-- | Name of the Amazon Kinesis Analytics application to delete.
deleteApplication_applicationName :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationName = Lens.lens (\DeleteApplication' {applicationName} -> applicationName) (\s@DeleteApplication' {} a -> s {applicationName = a} :: DeleteApplication)

-- | You can use the @DescribeApplication@ operation to get this value.
deleteApplication_createTimestamp :: Lens.Lens' DeleteApplication Prelude.UTCTime
deleteApplication_createTimestamp = Lens.lens (\DeleteApplication' {createTimestamp} -> createTimestamp) (\s@DeleteApplication' {} a -> s {createTimestamp = a} :: DeleteApplication) Prelude.. Prelude._Time

instance Prelude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplication

instance Prelude.NFData DeleteApplication

instance Prelude.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "KinesisAnalytics_20150814.DeleteApplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Prelude..= applicationName),
            Prelude.Just
              ("CreateTimestamp" Prelude..= createTimestamp)
          ]
      )

instance Prelude.ToPath DeleteApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteApplicationResponse
