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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application along with all associated versions and
-- configurations. The application versions will not be deleted from your
-- Amazon S3 bucket.
--
-- You cannot delete an application that has a running environment.
module Network.AWS.ElasticBeanstalk.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_terminateEnvByForce,
    deleteApplication_applicationName,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an application.
--
-- /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | When set to true, running environments will be terminated before
    -- deleting the application.
    terminateEnvByForce :: Prelude.Maybe Prelude.Bool,
    -- | The name of the application to delete.
    applicationName :: Prelude.Text
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
-- 'terminateEnvByForce', 'deleteApplication_terminateEnvByForce' - When set to true, running environments will be terminated before
-- deleting the application.
--
-- 'applicationName', 'deleteApplication_applicationName' - The name of the application to delete.
newDeleteApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication pApplicationName_ =
  DeleteApplication'
    { terminateEnvByForce =
        Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | When set to true, running environments will be terminated before
-- deleting the application.
deleteApplication_terminateEnvByForce :: Lens.Lens' DeleteApplication (Prelude.Maybe Prelude.Bool)
deleteApplication_terminateEnvByForce = Lens.lens (\DeleteApplication' {terminateEnvByForce} -> terminateEnvByForce) (\s@DeleteApplication' {} a -> s {terminateEnvByForce = a} :: DeleteApplication)

-- | The name of the application to delete.
deleteApplication_applicationName :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationName = Lens.lens (\DeleteApplication' {applicationName} -> applicationName) (\s@DeleteApplication' {} a -> s {applicationName = a} :: DeleteApplication)

instance Prelude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteApplicationResponse'

instance Prelude.Hashable DeleteApplication

instance Prelude.NFData DeleteApplication

instance Prelude.ToHeaders DeleteApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApplication where
  toQuery DeleteApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteApplication" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "TerminateEnvByForce" Prelude.=: terminateEnvByForce,
        "ApplicationName" Prelude.=: applicationName
      ]

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApplicationResponse ::
  DeleteApplicationResponse
newDeleteApplicationResponse =
  DeleteApplicationResponse'

instance Prelude.NFData DeleteApplicationResponse
