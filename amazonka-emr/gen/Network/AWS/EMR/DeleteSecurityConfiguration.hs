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
-- Module      : Network.AWS.EMR.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security configuration.
module Network.AWS.EMR.DeleteSecurityConfiguration
  ( -- * Creating a Request
    DeleteSecurityConfiguration (..),
    newDeleteSecurityConfiguration,

    -- * Request Lenses
    deleteSecurityConfiguration_name,

    -- * Destructuring the Response
    DeleteSecurityConfigurationResponse (..),
    newDeleteSecurityConfigurationResponse,

    -- * Response Lenses
    deleteSecurityConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSecurityConfiguration' smart constructor.
data DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSecurityConfiguration_name' - The name of the security configuration.
newDeleteSecurityConfiguration ::
  -- | 'name'
  Core.Text ->
  DeleteSecurityConfiguration
newDeleteSecurityConfiguration pName_ =
  DeleteSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
deleteSecurityConfiguration_name :: Lens.Lens' DeleteSecurityConfiguration Core.Text
deleteSecurityConfiguration_name = Lens.lens (\DeleteSecurityConfiguration' {name} -> name) (\s@DeleteSecurityConfiguration' {} a -> s {name = a} :: DeleteSecurityConfiguration)

instance Core.AWSRequest DeleteSecurityConfiguration where
  type
    AWSResponse DeleteSecurityConfiguration =
      DeleteSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSecurityConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSecurityConfiguration

instance Core.NFData DeleteSecurityConfiguration

instance Core.ToHeaders DeleteSecurityConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DeleteSecurityConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSecurityConfiguration where
  toJSON DeleteSecurityConfiguration' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteSecurityConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSecurityConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSecurityConfigurationResponse' smart constructor.
data DeleteSecurityConfigurationResponse = DeleteSecurityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSecurityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSecurityConfigurationResponse_httpStatus' - The response's http status code.
newDeleteSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSecurityConfigurationResponse
newDeleteSecurityConfigurationResponse pHttpStatus_ =
  DeleteSecurityConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSecurityConfigurationResponse_httpStatus :: Lens.Lens' DeleteSecurityConfigurationResponse Core.Int
deleteSecurityConfigurationResponse_httpStatus = Lens.lens (\DeleteSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteSecurityConfigurationResponse)

instance
  Core.NFData
    DeleteSecurityConfigurationResponse
