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
-- Module      : Network.AWS.Cloud9.UpdateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_name,
    updateEnvironment_description,
    updateEnvironment_environmentId,

    -- * Destructuring the Response
    UpdateEnvironmentResponse (..),
    newUpdateEnvironmentResponse,

    -- * Response Lenses
    updateEnvironmentResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | A replacement name for the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | Any new or replacement description for the environment.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the environment to change settings.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEnvironment_name' - A replacement name for the environment.
--
-- 'description', 'updateEnvironment_description' - Any new or replacement description for the environment.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The ID of the environment to change settings.
newUpdateEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  UpdateEnvironment
newUpdateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | A replacement name for the environment.
updateEnvironment_name :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_name = Lens.lens (\UpdateEnvironment' {name} -> name) (\s@UpdateEnvironment' {} a -> s {name = a} :: UpdateEnvironment)

-- | Any new or replacement description for the environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the environment to change settings.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

instance Prelude.AWSRequest UpdateEnvironment where
  type Rs UpdateEnvironment = UpdateEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEnvironment

instance Prelude.NFData UpdateEnvironment

instance Prelude.ToHeaders UpdateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("name" Prelude..=) Prelude.<$> name,
            ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just
              ("environmentId" Prelude..= environmentId)
          ]
      )

instance Prelude.ToPath UpdateEnvironment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentResponse' smart constructor.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEnvironmentResponse
newUpdateEnvironmentResponse pHttpStatus_ =
  UpdateEnvironmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEnvironmentResponse_httpStatus :: Lens.Lens' UpdateEnvironmentResponse Prelude.Int
updateEnvironmentResponse_httpStatus = Lens.lens (\UpdateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentResponse)

instance Prelude.NFData UpdateEnvironmentResponse
