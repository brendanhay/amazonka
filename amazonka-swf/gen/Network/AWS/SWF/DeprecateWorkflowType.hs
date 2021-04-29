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
-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /workflow type/. After a workflow type has been
-- deprecated, you cannot create new executions of that type. Executions
-- that were started before the type was deprecated continues to run. A
-- deprecated workflow type may still be used when calling visibility
-- actions.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
module Network.AWS.SWF.DeprecateWorkflowType
  ( -- * Creating a Request
    DeprecateWorkflowType (..),
    newDeprecateWorkflowType,

    -- * Request Lenses
    deprecateWorkflowType_domain,
    deprecateWorkflowType_workflowType,

    -- * Destructuring the Response
    DeprecateWorkflowTypeResponse (..),
    newDeprecateWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newDeprecateWorkflowType' smart constructor.
data DeprecateWorkflowType = DeprecateWorkflowType'
  { -- | The name of the domain in which the workflow type is registered.
    domain :: Prelude.Text,
    -- | The workflow type to deprecate.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateWorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'deprecateWorkflowType_domain' - The name of the domain in which the workflow type is registered.
--
-- 'workflowType', 'deprecateWorkflowType_workflowType' - The workflow type to deprecate.
newDeprecateWorkflowType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  DeprecateWorkflowType
newDeprecateWorkflowType pDomain_ pWorkflowType_ =
  DeprecateWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain in which the workflow type is registered.
deprecateWorkflowType_domain :: Lens.Lens' DeprecateWorkflowType Prelude.Text
deprecateWorkflowType_domain = Lens.lens (\DeprecateWorkflowType' {domain} -> domain) (\s@DeprecateWorkflowType' {} a -> s {domain = a} :: DeprecateWorkflowType)

-- | The workflow type to deprecate.
deprecateWorkflowType_workflowType :: Lens.Lens' DeprecateWorkflowType WorkflowType
deprecateWorkflowType_workflowType = Lens.lens (\DeprecateWorkflowType' {workflowType} -> workflowType) (\s@DeprecateWorkflowType' {} a -> s {workflowType = a} :: DeprecateWorkflowType)

instance Prelude.AWSRequest DeprecateWorkflowType where
  type
    Rs DeprecateWorkflowType =
      DeprecateWorkflowTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeprecateWorkflowTypeResponse'

instance Prelude.Hashable DeprecateWorkflowType

instance Prelude.NFData DeprecateWorkflowType

instance Prelude.ToHeaders DeprecateWorkflowType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.DeprecateWorkflowType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeprecateWorkflowType where
  toJSON DeprecateWorkflowType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just
              ("workflowType" Prelude..= workflowType)
          ]
      )

instance Prelude.ToPath DeprecateWorkflowType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeprecateWorkflowType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeprecateWorkflowTypeResponse' smart constructor.
data DeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateWorkflowTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeprecateWorkflowTypeResponse ::
  DeprecateWorkflowTypeResponse
newDeprecateWorkflowTypeResponse =
  DeprecateWorkflowTypeResponse'

instance Prelude.NFData DeprecateWorkflowTypeResponse
