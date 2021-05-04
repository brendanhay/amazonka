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
-- Module      : Network.AWS.SWF.UndeprecateWorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /workflow type/. After a workflow
-- type has been undeprecated, you can create new executions of that type.
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
module Network.AWS.SWF.UndeprecateWorkflowType
  ( -- * Creating a Request
    UndeprecateWorkflowType (..),
    newUndeprecateWorkflowType,

    -- * Request Lenses
    undeprecateWorkflowType_domain,
    undeprecateWorkflowType_workflowType,

    -- * Destructuring the Response
    UndeprecateWorkflowTypeResponse (..),
    newUndeprecateWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newUndeprecateWorkflowType' smart constructor.
data UndeprecateWorkflowType = UndeprecateWorkflowType'
  { -- | The name of the domain of the deprecated workflow type.
    domain :: Prelude.Text,
    -- | The name of the domain of the deprecated workflow type.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateWorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'undeprecateWorkflowType_domain' - The name of the domain of the deprecated workflow type.
--
-- 'workflowType', 'undeprecateWorkflowType_workflowType' - The name of the domain of the deprecated workflow type.
newUndeprecateWorkflowType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  UndeprecateWorkflowType
newUndeprecateWorkflowType pDomain_ pWorkflowType_ =
  UndeprecateWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain of the deprecated workflow type.
undeprecateWorkflowType_domain :: Lens.Lens' UndeprecateWorkflowType Prelude.Text
undeprecateWorkflowType_domain = Lens.lens (\UndeprecateWorkflowType' {domain} -> domain) (\s@UndeprecateWorkflowType' {} a -> s {domain = a} :: UndeprecateWorkflowType)

-- | The name of the domain of the deprecated workflow type.
undeprecateWorkflowType_workflowType :: Lens.Lens' UndeprecateWorkflowType WorkflowType
undeprecateWorkflowType_workflowType = Lens.lens (\UndeprecateWorkflowType' {workflowType} -> workflowType) (\s@UndeprecateWorkflowType' {} a -> s {workflowType = a} :: UndeprecateWorkflowType)

instance Prelude.AWSRequest UndeprecateWorkflowType where
  type
    Rs UndeprecateWorkflowType =
      UndeprecateWorkflowTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UndeprecateWorkflowTypeResponse'

instance Prelude.Hashable UndeprecateWorkflowType

instance Prelude.NFData UndeprecateWorkflowType

instance Prelude.ToHeaders UndeprecateWorkflowType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.UndeprecateWorkflowType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UndeprecateWorkflowType where
  toJSON UndeprecateWorkflowType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just
              ("workflowType" Prelude..= workflowType)
          ]
      )

instance Prelude.ToPath UndeprecateWorkflowType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UndeprecateWorkflowType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUndeprecateWorkflowTypeResponse' smart constructor.
data UndeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateWorkflowTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUndeprecateWorkflowTypeResponse ::
  UndeprecateWorkflowTypeResponse
newUndeprecateWorkflowTypeResponse =
  UndeprecateWorkflowTypeResponse'

instance
  Prelude.NFData
    UndeprecateWorkflowTypeResponse
