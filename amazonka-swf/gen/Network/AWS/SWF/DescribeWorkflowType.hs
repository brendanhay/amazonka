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
-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SWF.DescribeWorkflowType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newDescribeWorkflowType' smart constructor.
data DescribeWorkflowType = DescribeWorkflowType'
  { -- | The name of the domain in which this workflow type is registered.
    domain :: Core.Text,
    -- | The workflow type to describe.
    workflowType :: WorkflowType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'workflowType'
  WorkflowType ->
  DescribeWorkflowType
newDescribeWorkflowType pDomain_ pWorkflowType_ =
  DescribeWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain in which this workflow type is registered.
describeWorkflowType_domain :: Lens.Lens' DescribeWorkflowType Core.Text
describeWorkflowType_domain = Lens.lens (\DescribeWorkflowType' {domain} -> domain) (\s@DescribeWorkflowType' {} a -> s {domain = a} :: DescribeWorkflowType)

-- | The workflow type to describe.
describeWorkflowType_workflowType :: Lens.Lens' DescribeWorkflowType WorkflowType
describeWorkflowType_workflowType = Lens.lens (\DescribeWorkflowType' {workflowType} -> workflowType) (\s@DescribeWorkflowType' {} a -> s {workflowType = a} :: DescribeWorkflowType)

instance Core.AWSRequest DescribeWorkflowType where
  type
    AWSResponse DescribeWorkflowType =
      DescribeWorkflowTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkflowTypeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "typeInfo")
            Core.<*> (x Core..:> "configuration")
      )

instance Core.Hashable DescribeWorkflowType

instance Core.NFData DescribeWorkflowType

instance Core.ToHeaders DescribeWorkflowType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.DescribeWorkflowType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkflowType where
  toJSON DescribeWorkflowType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("workflowType" Core..= workflowType)
          ]
      )

instance Core.ToPath DescribeWorkflowType where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkflowType where
  toQuery = Core.const Core.mempty

-- | Contains details about a workflow type.
--
-- /See:/ 'newDescribeWorkflowTypeResponse' smart constructor.
data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
describeWorkflowTypeResponse_httpStatus :: Lens.Lens' DescribeWorkflowTypeResponse Core.Int
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

instance Core.NFData DescribeWorkflowTypeResponse
