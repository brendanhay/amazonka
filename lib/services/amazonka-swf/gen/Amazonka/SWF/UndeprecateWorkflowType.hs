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
-- Module      : Amazonka.SWF.UndeprecateWorkflowType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.SWF.UndeprecateWorkflowType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newUndeprecateWorkflowType' smart constructor.
data UndeprecateWorkflowType = UndeprecateWorkflowType'
  { -- | The name of the domain of the deprecated workflow type.
    domain :: Prelude.Text,
    -- | The name of the domain of the deprecated workflow type.
    workflowType :: WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UndeprecateWorkflowType where
  type
    AWSResponse UndeprecateWorkflowType =
      UndeprecateWorkflowTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UndeprecateWorkflowTypeResponse'

instance Prelude.Hashable UndeprecateWorkflowType where
  hashWithSalt _salt UndeprecateWorkflowType' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` workflowType

instance Prelude.NFData UndeprecateWorkflowType where
  rnf UndeprecateWorkflowType' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf workflowType

instance Data.ToHeaders UndeprecateWorkflowType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.UndeprecateWorkflowType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UndeprecateWorkflowType where
  toJSON UndeprecateWorkflowType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("workflowType" Data..= workflowType)
          ]
      )

instance Data.ToPath UndeprecateWorkflowType where
  toPath = Prelude.const "/"

instance Data.ToQuery UndeprecateWorkflowType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUndeprecateWorkflowTypeResponse' smart constructor.
data UndeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
