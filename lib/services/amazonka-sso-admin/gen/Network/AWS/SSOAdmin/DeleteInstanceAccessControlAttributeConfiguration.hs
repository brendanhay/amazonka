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
-- Module      : Network.AWS.SSOAdmin.DeleteInstanceAccessControlAttributeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the attributes-based access control (ABAC) feature for the
-- specified Amazon Web Services SSO instance and deletes all of the
-- attribute mappings that have been configured. Once deleted, any
-- attributes that are received from an identity source and any custom
-- attributes you have previously configured will not be passed. For more
-- information about ABAC, see
-- </singlesignon/latest/userguide/abac.html Attribute-Based Access Control>
-- in the /Amazon Web Services SSO User Guide/.
module Network.AWS.SSOAdmin.DeleteInstanceAccessControlAttributeConfiguration
  ( -- * Creating a Request
    DeleteInstanceAccessControlAttributeConfiguration (..),
    newDeleteInstanceAccessControlAttributeConfiguration,

    -- * Request Lenses
    deleteInstanceAccessControlAttributeConfiguration_instanceArn,

    -- * Destructuring the Response
    DeleteInstanceAccessControlAttributeConfigurationResponse (..),
    newDeleteInstanceAccessControlAttributeConfigurationResponse,

    -- * Response Lenses
    deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSOAdmin.Types

-- | /See:/ 'newDeleteInstanceAccessControlAttributeConfiguration' smart constructor.
data DeleteInstanceAccessControlAttributeConfiguration = DeleteInstanceAccessControlAttributeConfiguration'
  { -- | The ARN of the SSO instance under which the operation will be executed.
    instanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceAccessControlAttributeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'deleteInstanceAccessControlAttributeConfiguration_instanceArn' - The ARN of the SSO instance under which the operation will be executed.
newDeleteInstanceAccessControlAttributeConfiguration ::
  -- | 'instanceArn'
  Prelude.Text ->
  DeleteInstanceAccessControlAttributeConfiguration
newDeleteInstanceAccessControlAttributeConfiguration
  pInstanceArn_ =
    DeleteInstanceAccessControlAttributeConfiguration'
      { instanceArn =
          pInstanceArn_
      }

-- | The ARN of the SSO instance under which the operation will be executed.
deleteInstanceAccessControlAttributeConfiguration_instanceArn :: Lens.Lens' DeleteInstanceAccessControlAttributeConfiguration Prelude.Text
deleteInstanceAccessControlAttributeConfiguration_instanceArn = Lens.lens (\DeleteInstanceAccessControlAttributeConfiguration' {instanceArn} -> instanceArn) (\s@DeleteInstanceAccessControlAttributeConfiguration' {} a -> s {instanceArn = a} :: DeleteInstanceAccessControlAttributeConfiguration)

instance
  Core.AWSRequest
    DeleteInstanceAccessControlAttributeConfiguration
  where
  type
    AWSResponse
      DeleteInstanceAccessControlAttributeConfiguration =
      DeleteInstanceAccessControlAttributeConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInstanceAccessControlAttributeConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteInstanceAccessControlAttributeConfiguration

instance
  Prelude.NFData
    DeleteInstanceAccessControlAttributeConfiguration

instance
  Core.ToHeaders
    DeleteInstanceAccessControlAttributeConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.DeleteInstanceAccessControlAttributeConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteInstanceAccessControlAttributeConfiguration
  where
  toJSON
    DeleteInstanceAccessControlAttributeConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [Prelude.Just ("InstanceArn" Core..= instanceArn)]
        )

instance
  Core.ToPath
    DeleteInstanceAccessControlAttributeConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteInstanceAccessControlAttributeConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceAccessControlAttributeConfigurationResponse' smart constructor.
data DeleteInstanceAccessControlAttributeConfigurationResponse = DeleteInstanceAccessControlAttributeConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceAccessControlAttributeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus' - The response's http status code.
newDeleteInstanceAccessControlAttributeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInstanceAccessControlAttributeConfigurationResponse
newDeleteInstanceAccessControlAttributeConfigurationResponse
  pHttpStatus_ =
    DeleteInstanceAccessControlAttributeConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus :: Lens.Lens' DeleteInstanceAccessControlAttributeConfigurationResponse Prelude.Int
deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus = Lens.lens (\DeleteInstanceAccessControlAttributeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceAccessControlAttributeConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteInstanceAccessControlAttributeConfigurationResponse)

instance
  Prelude.NFData
    DeleteInstanceAccessControlAttributeConfigurationResponse
