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
-- Module      : Amazonka.SSOAdmin.CreateInstanceAccessControlAttributeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the attributes-based access control (ABAC) feature for the
-- specified IAM Identity Center instance. You can also specify new
-- attributes to add to your ABAC configuration during the enabling
-- process. For more information about ABAC, see
-- </singlesignon/latest/userguide/abac.html Attribute-Based Access Control>
-- in the /IAM Identity Center User Guide/.
--
-- After a successful response, call
-- @DescribeInstanceAccessControlAttributeConfiguration@ to validate that
-- @InstanceAccessControlAttributeConfiguration@ was created.
module Amazonka.SSOAdmin.CreateInstanceAccessControlAttributeConfiguration
  ( -- * Creating a Request
    CreateInstanceAccessControlAttributeConfiguration (..),
    newCreateInstanceAccessControlAttributeConfiguration,

    -- * Request Lenses
    createInstanceAccessControlAttributeConfiguration_instanceArn,
    createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,

    -- * Destructuring the Response
    CreateInstanceAccessControlAttributeConfigurationResponse (..),
    newCreateInstanceAccessControlAttributeConfigurationResponse,

    -- * Response Lenses
    createInstanceAccessControlAttributeConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newCreateInstanceAccessControlAttributeConfiguration' smart constructor.
data CreateInstanceAccessControlAttributeConfiguration = CreateInstanceAccessControlAttributeConfiguration'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | Specifies the IAM Identity Center identity store attributes to add to
    -- your ABAC configuration. When using an external identity provider as an
    -- identity source, you can pass attributes through the SAML assertion.
    -- Doing so provides an alternative to configuring attributes from the IAM
    -- Identity Center identity store. If a SAML assertion passes any of these
    -- attributes, IAM Identity Center will replace the attribute value with
    -- the value from the IAM Identity Center identity store.
    instanceAccessControlAttributeConfiguration :: InstanceAccessControlAttributeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceAccessControlAttributeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'createInstanceAccessControlAttributeConfiguration_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'instanceAccessControlAttributeConfiguration', 'createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration' - Specifies the IAM Identity Center identity store attributes to add to
-- your ABAC configuration. When using an external identity provider as an
-- identity source, you can pass attributes through the SAML assertion.
-- Doing so provides an alternative to configuring attributes from the IAM
-- Identity Center identity store. If a SAML assertion passes any of these
-- attributes, IAM Identity Center will replace the attribute value with
-- the value from the IAM Identity Center identity store.
newCreateInstanceAccessControlAttributeConfiguration ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'instanceAccessControlAttributeConfiguration'
  InstanceAccessControlAttributeConfiguration ->
  CreateInstanceAccessControlAttributeConfiguration
newCreateInstanceAccessControlAttributeConfiguration
  pInstanceArn_
  pInstanceAccessControlAttributeConfiguration_ =
    CreateInstanceAccessControlAttributeConfiguration'
      { instanceArn =
          pInstanceArn_,
        instanceAccessControlAttributeConfiguration =
          pInstanceAccessControlAttributeConfiguration_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
createInstanceAccessControlAttributeConfiguration_instanceArn :: Lens.Lens' CreateInstanceAccessControlAttributeConfiguration Prelude.Text
createInstanceAccessControlAttributeConfiguration_instanceArn = Lens.lens (\CreateInstanceAccessControlAttributeConfiguration' {instanceArn} -> instanceArn) (\s@CreateInstanceAccessControlAttributeConfiguration' {} a -> s {instanceArn = a} :: CreateInstanceAccessControlAttributeConfiguration)

-- | Specifies the IAM Identity Center identity store attributes to add to
-- your ABAC configuration. When using an external identity provider as an
-- identity source, you can pass attributes through the SAML assertion.
-- Doing so provides an alternative to configuring attributes from the IAM
-- Identity Center identity store. If a SAML assertion passes any of these
-- attributes, IAM Identity Center will replace the attribute value with
-- the value from the IAM Identity Center identity store.
createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration :: Lens.Lens' CreateInstanceAccessControlAttributeConfiguration InstanceAccessControlAttributeConfiguration
createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration = Lens.lens (\CreateInstanceAccessControlAttributeConfiguration' {instanceAccessControlAttributeConfiguration} -> instanceAccessControlAttributeConfiguration) (\s@CreateInstanceAccessControlAttributeConfiguration' {} a -> s {instanceAccessControlAttributeConfiguration = a} :: CreateInstanceAccessControlAttributeConfiguration)

instance
  Core.AWSRequest
    CreateInstanceAccessControlAttributeConfiguration
  where
  type
    AWSResponse
      CreateInstanceAccessControlAttributeConfiguration =
      CreateInstanceAccessControlAttributeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateInstanceAccessControlAttributeConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateInstanceAccessControlAttributeConfiguration
  where
  hashWithSalt
    _salt
    CreateInstanceAccessControlAttributeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` instanceAccessControlAttributeConfiguration

instance
  Prelude.NFData
    CreateInstanceAccessControlAttributeConfiguration
  where
  rnf
    CreateInstanceAccessControlAttributeConfiguration' {..} =
      Prelude.rnf instanceArn
        `Prelude.seq` Prelude.rnf
          instanceAccessControlAttributeConfiguration

instance
  Data.ToHeaders
    CreateInstanceAccessControlAttributeConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.CreateInstanceAccessControlAttributeConfiguration" ::
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
    CreateInstanceAccessControlAttributeConfiguration
  where
  toJSON
    CreateInstanceAccessControlAttributeConfiguration' {..} =
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
    CreateInstanceAccessControlAttributeConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateInstanceAccessControlAttributeConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstanceAccessControlAttributeConfigurationResponse' smart constructor.
data CreateInstanceAccessControlAttributeConfigurationResponse = CreateInstanceAccessControlAttributeConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceAccessControlAttributeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInstanceAccessControlAttributeConfigurationResponse_httpStatus' - The response's http status code.
newCreateInstanceAccessControlAttributeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceAccessControlAttributeConfigurationResponse
newCreateInstanceAccessControlAttributeConfigurationResponse
  pHttpStatus_ =
    CreateInstanceAccessControlAttributeConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createInstanceAccessControlAttributeConfigurationResponse_httpStatus :: Lens.Lens' CreateInstanceAccessControlAttributeConfigurationResponse Prelude.Int
createInstanceAccessControlAttributeConfigurationResponse_httpStatus = Lens.lens (\CreateInstanceAccessControlAttributeConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceAccessControlAttributeConfigurationResponse' {} a -> s {httpStatus = a} :: CreateInstanceAccessControlAttributeConfigurationResponse)

instance
  Prelude.NFData
    CreateInstanceAccessControlAttributeConfigurationResponse
  where
  rnf
    CreateInstanceAccessControlAttributeConfigurationResponse' {..} =
      Prelude.rnf httpStatus
