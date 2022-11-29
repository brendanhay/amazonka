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
-- Module      : Amazonka.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified /workflow type/. This includes
-- configuration settings specified when the type was registered and other
-- information such as creation date, current status, etc.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @workflowType.name@: String constraint. The key is
--         @swf:workflowType.name@.
--
--     -   @workflowType.version@: String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.DescribeWorkflowType
  ( -- * Creating a Request
    DescribeWorkflowType (..),
    newDescribeWorkflowType,

    -- * Request Lenses
    describeWorkflowType_domain,
    describeWorkflowType_workflowType,

    -- * Destructuring the Response
    DescribeWorkflowTypeResponse (..),
    newDescribeWorkflowTypeResponse,

    -- * Response Lenses
    describeWorkflowTypeResponse_httpStatus,
    describeWorkflowTypeResponse_typeInfo,
    describeWorkflowTypeResponse_configuration,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newDescribeWorkflowType' smart constructor.
data DescribeWorkflowType = DescribeWorkflowType'
  { -- | The name of the domain in which this workflow type is registered.
    domain :: Prelude.Text,
    -- | The workflow type to describe.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'describeWorkflowType_domain' - The name of the domain in which this workflow type is registered.
--
-- 'workflowType', 'describeWorkflowType_workflowType' - The workflow type to describe.
newDescribeWorkflowType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  DescribeWorkflowType
newDescribeWorkflowType pDomain_ pWorkflowType_ =
  DescribeWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain in which this workflow type is registered.
describeWorkflowType_domain :: Lens.Lens' DescribeWorkflowType Prelude.Text
describeWorkflowType_domain = Lens.lens (\DescribeWorkflowType' {domain} -> domain) (\s@DescribeWorkflowType' {} a -> s {domain = a} :: DescribeWorkflowType)

-- | The workflow type to describe.
describeWorkflowType_workflowType :: Lens.Lens' DescribeWorkflowType WorkflowType
describeWorkflowType_workflowType = Lens.lens (\DescribeWorkflowType' {workflowType} -> workflowType) (\s@DescribeWorkflowType' {} a -> s {workflowType = a} :: DescribeWorkflowType)

instance Core.AWSRequest DescribeWorkflowType where
  type
    AWSResponse DescribeWorkflowType =
      DescribeWorkflowTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkflowTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "typeInfo")
            Prelude.<*> (x Core..:> "configuration")
      )

instance Prelude.Hashable DescribeWorkflowType where
  hashWithSalt _salt DescribeWorkflowType' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` workflowType

instance Prelude.NFData DescribeWorkflowType where
  rnf DescribeWorkflowType' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf workflowType

instance Core.ToHeaders DescribeWorkflowType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.DescribeWorkflowType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkflowType where
  toJSON DescribeWorkflowType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Core..= domain),
            Prelude.Just ("workflowType" Core..= workflowType)
          ]
      )

instance Core.ToPath DescribeWorkflowType where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkflowType where
  toQuery = Prelude.const Prelude.mempty

-- | Contains details about a workflow type.
--
-- /See:/ 'newDescribeWorkflowTypeResponse' smart constructor.
data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | General information about the workflow type.
    --
    -- The status of the workflow type (returned in the WorkflowTypeInfo
    -- structure) can be one of the following.
    --
    -- -   @REGISTERED@ – The type is registered and available. Workers
    --     supporting this type should be running.
    --
    -- -   @DEPRECATED@ – The type was deprecated using DeprecateWorkflowType,
    --     but is still in use. You should keep workers supporting this type
    --     running. You cannot create new workflow executions of this type.
    typeInfo :: WorkflowTypeInfo,
    -- | Configuration settings of the workflow type registered through
    -- RegisterWorkflowType
    configuration :: WorkflowTypeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkflowTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkflowTypeResponse_httpStatus' - The response's http status code.
--
-- 'typeInfo', 'describeWorkflowTypeResponse_typeInfo' - General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo
-- structure) can be one of the following.
--
-- -   @REGISTERED@ – The type is registered and available. Workers
--     supporting this type should be running.
--
-- -   @DEPRECATED@ – The type was deprecated using DeprecateWorkflowType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new workflow executions of this type.
--
-- 'configuration', 'describeWorkflowTypeResponse_configuration' - Configuration settings of the workflow type registered through
-- RegisterWorkflowType
newDescribeWorkflowTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'typeInfo'
  WorkflowTypeInfo ->
  -- | 'configuration'
  WorkflowTypeConfiguration ->
  DescribeWorkflowTypeResponse
newDescribeWorkflowTypeResponse
  pHttpStatus_
  pTypeInfo_
  pConfiguration_ =
    DescribeWorkflowTypeResponse'
      { httpStatus =
          pHttpStatus_,
        typeInfo = pTypeInfo_,
        configuration = pConfiguration_
      }

-- | The response's http status code.
describeWorkflowTypeResponse_httpStatus :: Lens.Lens' DescribeWorkflowTypeResponse Prelude.Int
describeWorkflowTypeResponse_httpStatus = Lens.lens (\DescribeWorkflowTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkflowTypeResponse' {} a -> s {httpStatus = a} :: DescribeWorkflowTypeResponse)

-- | General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo
-- structure) can be one of the following.
--
-- -   @REGISTERED@ – The type is registered and available. Workers
--     supporting this type should be running.
--
-- -   @DEPRECATED@ – The type was deprecated using DeprecateWorkflowType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new workflow executions of this type.
describeWorkflowTypeResponse_typeInfo :: Lens.Lens' DescribeWorkflowTypeResponse WorkflowTypeInfo
describeWorkflowTypeResponse_typeInfo = Lens.lens (\DescribeWorkflowTypeResponse' {typeInfo} -> typeInfo) (\s@DescribeWorkflowTypeResponse' {} a -> s {typeInfo = a} :: DescribeWorkflowTypeResponse)

-- | Configuration settings of the workflow type registered through
-- RegisterWorkflowType
describeWorkflowTypeResponse_configuration :: Lens.Lens' DescribeWorkflowTypeResponse WorkflowTypeConfiguration
describeWorkflowTypeResponse_configuration = Lens.lens (\DescribeWorkflowTypeResponse' {configuration} -> configuration) (\s@DescribeWorkflowTypeResponse' {} a -> s {configuration = a} :: DescribeWorkflowTypeResponse)

instance Prelude.NFData DescribeWorkflowTypeResponse where
  rnf DescribeWorkflowTypeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf typeInfo
      `Prelude.seq` Prelude.rnf configuration
