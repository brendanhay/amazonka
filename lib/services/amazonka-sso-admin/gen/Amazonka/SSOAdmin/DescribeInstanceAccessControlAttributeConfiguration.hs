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
-- Module      : Amazonka.SSOAdmin.DescribeInstanceAccessControlAttributeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of IAM Identity Center identity store attributes that
-- have been configured to work with attributes-based access control (ABAC)
-- for the specified IAM Identity Center instance. This will not return
-- attributes configured and sent by an external identity provider. For
-- more information about ABAC, see
-- </singlesignon/latest/userguide/abac.html Attribute-Based Access Control>
-- in the /IAM Identity Center User Guide/.
module Amazonka.SSOAdmin.DescribeInstanceAccessControlAttributeConfiguration
  ( -- * Creating a Request
    DescribeInstanceAccessControlAttributeConfiguration (..),
    newDescribeInstanceAccessControlAttributeConfiguration,

    -- * Request Lenses
    describeInstanceAccessControlAttributeConfiguration_instanceArn,

    -- * Destructuring the Response
    DescribeInstanceAccessControlAttributeConfigurationResponse (..),
    newDescribeInstanceAccessControlAttributeConfigurationResponse,

    -- * Response Lenses
    describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration,
    describeInstanceAccessControlAttributeConfigurationResponse_statusReason,
    describeInstanceAccessControlAttributeConfigurationResponse_status,
    describeInstanceAccessControlAttributeConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDescribeInstanceAccessControlAttributeConfiguration' smart constructor.
data DescribeInstanceAccessControlAttributeConfiguration = DescribeInstanceAccessControlAttributeConfiguration'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAccessControlAttributeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'describeInstanceAccessControlAttributeConfiguration_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
newDescribeInstanceAccessControlAttributeConfiguration ::
  -- | 'instanceArn'
  Prelude.Text ->
  DescribeInstanceAccessControlAttributeConfiguration
newDescribeInstanceAccessControlAttributeConfiguration
  pInstanceArn_ =
    DescribeInstanceAccessControlAttributeConfiguration'
      { instanceArn =
          pInstanceArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
describeInstanceAccessControlAttributeConfiguration_instanceArn :: Lens.Lens' DescribeInstanceAccessControlAttributeConfiguration Prelude.Text
describeInstanceAccessControlAttributeConfiguration_instanceArn = Lens.lens (\DescribeInstanceAccessControlAttributeConfiguration' {instanceArn} -> instanceArn) (\s@DescribeInstanceAccessControlAttributeConfiguration' {} a -> s {instanceArn = a} :: DescribeInstanceAccessControlAttributeConfiguration)

instance
  Core.AWSRequest
    DescribeInstanceAccessControlAttributeConfiguration
  where
  type
    AWSResponse
      DescribeInstanceAccessControlAttributeConfiguration =
      DescribeInstanceAccessControlAttributeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAccessControlAttributeConfigurationResponse'
            Prelude.<$> ( x
                            Core..?> "InstanceAccessControlAttributeConfiguration"
                        )
              Prelude.<*> (x Core..?> "StatusReason")
              Prelude.<*> (x Core..?> "Status")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceAccessControlAttributeConfiguration
  where
  hashWithSalt
    _salt
    DescribeInstanceAccessControlAttributeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` instanceArn

instance
  Prelude.NFData
    DescribeInstanceAccessControlAttributeConfiguration
  where
  rnf
    DescribeInstanceAccessControlAttributeConfiguration' {..} =
      Prelude.rnf instanceArn

instance
  Core.ToHeaders
    DescribeInstanceAccessControlAttributeConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.DescribeInstanceAccessControlAttributeConfiguration" ::
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
    DescribeInstanceAccessControlAttributeConfiguration
  where
  toJSON
    DescribeInstanceAccessControlAttributeConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [Prelude.Just ("InstanceArn" Core..= instanceArn)]
        )

instance
  Core.ToPath
    DescribeInstanceAccessControlAttributeConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeInstanceAccessControlAttributeConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceAccessControlAttributeConfigurationResponse' smart constructor.
data DescribeInstanceAccessControlAttributeConfigurationResponse = DescribeInstanceAccessControlAttributeConfigurationResponse'
  { -- | Gets the list of IAM Identity Center identity store attributes that have
    -- been added to your ABAC configuration.
    instanceAccessControlAttributeConfiguration :: Prelude.Maybe InstanceAccessControlAttributeConfiguration,
    -- | Provides more details about the current status of the specified
    -- attribute.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the attribute configuration process.
    status :: Prelude.Maybe InstanceAccessControlAttributeConfigurationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAccessControlAttributeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceAccessControlAttributeConfiguration', 'describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration' - Gets the list of IAM Identity Center identity store attributes that have
-- been added to your ABAC configuration.
--
-- 'statusReason', 'describeInstanceAccessControlAttributeConfigurationResponse_statusReason' - Provides more details about the current status of the specified
-- attribute.
--
-- 'status', 'describeInstanceAccessControlAttributeConfigurationResponse_status' - The status of the attribute configuration process.
--
-- 'httpStatus', 'describeInstanceAccessControlAttributeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeInstanceAccessControlAttributeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceAccessControlAttributeConfigurationResponse
newDescribeInstanceAccessControlAttributeConfigurationResponse
  pHttpStatus_ =
    DescribeInstanceAccessControlAttributeConfigurationResponse'
      { instanceAccessControlAttributeConfiguration =
          Prelude.Nothing,
        statusReason =
          Prelude.Nothing,
        status =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Gets the list of IAM Identity Center identity store attributes that have
-- been added to your ABAC configuration.
describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration :: Lens.Lens' DescribeInstanceAccessControlAttributeConfigurationResponse (Prelude.Maybe InstanceAccessControlAttributeConfiguration)
describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration = Lens.lens (\DescribeInstanceAccessControlAttributeConfigurationResponse' {instanceAccessControlAttributeConfiguration} -> instanceAccessControlAttributeConfiguration) (\s@DescribeInstanceAccessControlAttributeConfigurationResponse' {} a -> s {instanceAccessControlAttributeConfiguration = a} :: DescribeInstanceAccessControlAttributeConfigurationResponse)

-- | Provides more details about the current status of the specified
-- attribute.
describeInstanceAccessControlAttributeConfigurationResponse_statusReason :: Lens.Lens' DescribeInstanceAccessControlAttributeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeInstanceAccessControlAttributeConfigurationResponse_statusReason = Lens.lens (\DescribeInstanceAccessControlAttributeConfigurationResponse' {statusReason} -> statusReason) (\s@DescribeInstanceAccessControlAttributeConfigurationResponse' {} a -> s {statusReason = a} :: DescribeInstanceAccessControlAttributeConfigurationResponse)

-- | The status of the attribute configuration process.
describeInstanceAccessControlAttributeConfigurationResponse_status :: Lens.Lens' DescribeInstanceAccessControlAttributeConfigurationResponse (Prelude.Maybe InstanceAccessControlAttributeConfigurationStatus)
describeInstanceAccessControlAttributeConfigurationResponse_status = Lens.lens (\DescribeInstanceAccessControlAttributeConfigurationResponse' {status} -> status) (\s@DescribeInstanceAccessControlAttributeConfigurationResponse' {} a -> s {status = a} :: DescribeInstanceAccessControlAttributeConfigurationResponse)

-- | The response's http status code.
describeInstanceAccessControlAttributeConfigurationResponse_httpStatus :: Lens.Lens' DescribeInstanceAccessControlAttributeConfigurationResponse Prelude.Int
describeInstanceAccessControlAttributeConfigurationResponse_httpStatus = Lens.lens (\DescribeInstanceAccessControlAttributeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAccessControlAttributeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAccessControlAttributeConfigurationResponse)

instance
  Prelude.NFData
    DescribeInstanceAccessControlAttributeConfigurationResponse
  where
  rnf
    DescribeInstanceAccessControlAttributeConfigurationResponse' {..} =
      Prelude.rnf
        instanceAccessControlAttributeConfiguration
        `Prelude.seq` Prelude.rnf statusReason
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf httpStatus
