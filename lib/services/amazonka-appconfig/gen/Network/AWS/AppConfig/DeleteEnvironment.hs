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
-- Module      : Network.AWS.AppConfig.DeleteEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an environment. Deleting an environment does not delete a
-- configuration from a host.
module Network.AWS.AppConfig.DeleteEnvironment
  ( -- * Creating a Request
    DeleteEnvironment (..),
    newDeleteEnvironment,

    -- * Request Lenses
    deleteEnvironment_applicationId,
    deleteEnvironment_environmentId,

    -- * Destructuring the Response
    DeleteEnvironmentResponse (..),
    newDeleteEnvironmentResponse,
  )
where

import Network.AWS.AppConfig.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEnvironment' smart constructor.
data DeleteEnvironment = DeleteEnvironment'
  { -- | The application ID that includes the environment you want to delete.
    applicationId :: Prelude.Text,
    -- | The ID of the environment you want to delete.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteEnvironment_applicationId' - The application ID that includes the environment you want to delete.
--
-- 'environmentId', 'deleteEnvironment_environmentId' - The ID of the environment you want to delete.
newDeleteEnvironment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  DeleteEnvironment
newDeleteEnvironment pApplicationId_ pEnvironmentId_ =
  DeleteEnvironment'
    { applicationId = pApplicationId_,
      environmentId = pEnvironmentId_
    }

-- | The application ID that includes the environment you want to delete.
deleteEnvironment_applicationId :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_applicationId = Lens.lens (\DeleteEnvironment' {applicationId} -> applicationId) (\s@DeleteEnvironment' {} a -> s {applicationId = a} :: DeleteEnvironment)

-- | The ID of the environment you want to delete.
deleteEnvironment_environmentId :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_environmentId = Lens.lens (\DeleteEnvironment' {environmentId} -> environmentId) (\s@DeleteEnvironment' {} a -> s {environmentId = a} :: DeleteEnvironment)

instance Core.AWSRequest DeleteEnvironment where
  type
    AWSResponse DeleteEnvironment =
      DeleteEnvironmentResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteEnvironmentResponse'

instance Prelude.Hashable DeleteEnvironment

instance Prelude.NFData DeleteEnvironment

instance Core.ToHeaders DeleteEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEnvironment where
  toPath DeleteEnvironment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/environments/",
        Core.toBS environmentId
      ]

instance Core.ToQuery DeleteEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentResponse' smart constructor.
data DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEnvironmentResponse ::
  DeleteEnvironmentResponse
newDeleteEnvironmentResponse =
  DeleteEnvironmentResponse'

instance Prelude.NFData DeleteEnvironmentResponse
