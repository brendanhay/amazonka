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
-- Module      : Amazonka.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IAM Identity Center identity store attributes that you can
-- use with the IAM Identity Center instance for attributes-based access
-- control (ABAC). When using an external identity provider as an identity
-- source, you can pass attributes through the SAML assertion as an
-- alternative to configuring attributes from the IAM Identity Center
-- identity store. If a SAML assertion passes any of these attributes, IAM
-- Identity Center replaces the attribute value with the value from the IAM
-- Identity Center identity store. For more information about ABAC, see
-- </singlesignon/latest/userguide/abac.html Attribute-Based Access Control>
-- in the /IAM Identity Center User Guide/.
module Amazonka.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
  ( -- * Creating a Request
    UpdateInstanceAccessControlAttributeConfiguration (..),
    newUpdateInstanceAccessControlAttributeConfiguration,

    -- * Request Lenses
    updateInstanceAccessControlAttributeConfiguration_instanceArn,
    updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,

    -- * Destructuring the Response
    UpdateInstanceAccessControlAttributeConfigurationResponse (..),
    newUpdateInstanceAccessControlAttributeConfigurationResponse,

    -- * Response Lenses
    updateInstanceAccessControlAttributeConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newUpdateInstanceAccessControlAttributeConfiguration' smart constructor.
data UpdateInstanceAccessControlAttributeConfiguration = UpdateInstanceAccessControlAttributeConfiguration'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | Updates the attributes for your ABAC configuration.
    instanceAccessControlAttributeConfiguration :: InstanceAccessControlAttributeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceAccessControlAttributeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'updateInstanceAccessControlAttributeConfiguration_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'instanceAccessControlAttributeConfiguration', 'updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration' - Updates the attributes for your ABAC configuration.
newUpdateInstanceAccessControlAttributeConfiguration ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'instanceAccessControlAttributeConfiguration'
  InstanceAccessControlAttributeConfiguration ->
  UpdateInstanceAccessControlAttributeConfiguration
newUpdateInstanceAccessControlAttributeConfiguration
  pInstanceArn_
  pInstanceAccessControlAttributeConfiguration_ =
    UpdateInstanceAccessControlAttributeConfiguration'
      { instanceArn =
          pInstanceArn_,
        instanceAccessControlAttributeConfiguration =
          pInstanceAccessControlAttributeConfiguration_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
updateInstanceAccessControlAttributeConfiguration_instanceArn :: Lens.Lens' UpdateInstanceAccessControlAttributeConfiguration Prelude.Text
updateInstanceAccessControlAttributeConfiguration_instanceArn = Lens.lens (\UpdateInstanceAccessControlAttributeConfiguration' {instanceArn} -> instanceArn) (\s@UpdateInstanceAccessControlAttributeConfiguration' {} a -> s {instanceArn = a} :: UpdateInstanceAccessControlAttributeConfiguration)

-- | Updates the attributes for your ABAC configuration.
updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration :: Lens.Lens' UpdateInstanceAccessControlAttributeConfiguration InstanceAccessControlAttributeConfiguration
updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration = Lens.lens (\UpdateInstanceAccessControlAttributeConfiguration' {instanceAccessControlAttributeConfiguration} -> instanceAccessControlAttributeConfiguration) (\s@UpdateInstanceAccessControlAttributeConfiguration' {} a -> s {instanceAccessControlAttributeConfiguration = a} :: UpdateInstanceAccessControlAttributeConfiguration)

instance
  Core.AWSRequest
    UpdateInstanceAccessControlAttributeConfiguration
  where
  type
    AWSResponse
      UpdateInstanceAccessControlAttributeConfiguration =
      UpdateInstanceAccessControlAttributeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateInstanceAccessControlAttributeConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateInstanceAccessControlAttributeConfiguration
  where
  hashWithSalt
    _salt
    UpdateInstanceAccessControlAttributeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` instanceAccessControlAttributeConfiguration

instance
  Prelude.NFData
    UpdateInstanceAccessControlAttributeConfiguration
  where
  rnf
    UpdateInstanceAccessControlAttributeConfiguration' {..} =
      Prelude.rnf instanceArn
        `Prelude.seq` Prelude.rnf
          instanceAccessControlAttributeConfiguration

instance
  Data.ToHeaders
    UpdateInstanceAccessControlAttributeConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.UpdateInstanceAccessControlAttributeConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateInstanceAccessControlAttributeConfiguration
  where
  toJSON
    UpdateInstanceAccessControlAttributeConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("InstanceArn" Data..= instanceArn),
              Prelude.Just
                ( "InstanceAccessControlAttributeConfiguration"
                    Data..= instanceAccessControlAttributeConfiguration
                )
            ]
        )

instance
  Data.ToPath
    UpdateInstanceAccessControlAttributeConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateInstanceAccessControlAttributeConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceAccessControlAttributeConfigurationResponse' smart constructor.
data UpdateInstanceAccessControlAttributeConfigurationResponse = UpdateInstanceAccessControlAttributeConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceAccessControlAttributeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateInstanceAccessControlAttributeConfigurationResponse_httpStatus' - The response's http status code.
newUpdateInstanceAccessControlAttributeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInstanceAccessControlAttributeConfigurationResponse
newUpdateInstanceAccessControlAttributeConfigurationResponse
  pHttpStatus_ =
    UpdateInstanceAccessControlAttributeConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateInstanceAccessControlAttributeConfigurationResponse_httpStatus :: Lens.Lens' UpdateInstanceAccessControlAttributeConfigurationResponse Prelude.Int
updateInstanceAccessControlAttributeConfigurationResponse_httpStatus = Lens.lens (\UpdateInstanceAccessControlAttributeConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateInstanceAccessControlAttributeConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateInstanceAccessControlAttributeConfigurationResponse)

instance
  Prelude.NFData
    UpdateInstanceAccessControlAttributeConfigurationResponse
  where
  rnf
    UpdateInstanceAccessControlAttributeConfigurationResponse' {..} =
      Prelude.rnf httpStatus
