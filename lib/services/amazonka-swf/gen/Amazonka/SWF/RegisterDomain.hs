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
-- Module      : Amazonka.SWF.RegisterDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   You cannot use an IAM policy to control domain access for this
--     action. The name of the domain being registered is available as the
--     resource of this action.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.RegisterDomain
  ( -- * Creating a Request
    RegisterDomain (..),
    newRegisterDomain,

    -- * Request Lenses
    registerDomain_description,
    registerDomain_tags,
    registerDomain_name,
    registerDomain_workflowExecutionRetentionPeriodInDays,

    -- * Destructuring the Response
    RegisterDomainResponse (..),
    newRegisterDomainResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { -- | A text description of the domain.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags to be added when registering a domain.
    --
    -- Tags may only contain unicode letters, digits, whitespace, or these
    -- symbols: @_ . : \/ = + - \@@.
    tags :: Prelude.Maybe [ResourceTag],
    -- | Name of the domain to register. The name must be unique in the region
    -- that the domain is registered in.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must /not/
    -- be the literal string @arn@.
    name :: Prelude.Text,
    -- | The duration (in days) that records and histories of workflow executions
    -- on the domain should be kept by the service. After the retention period,
    -- the workflow execution isn\'t available in the results of visibility
    -- calls.
    --
    -- If you pass the value @NONE@ or @0@ (zero), then the workflow execution
    -- history isn\'t retained. As soon as the workflow execution completes,
    -- the execution record and its history are deleted.
    --
    -- The maximum workflow execution retention period is 90 days. For more
    -- information about Amazon SWF service limits, see:
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits>
    -- in the /Amazon SWF Developer Guide/.
    workflowExecutionRetentionPeriodInDays :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'registerDomain_description' - A text description of the domain.
--
-- 'tags', 'registerDomain_tags' - Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
--
-- 'name', 'registerDomain_name' - Name of the domain to register. The name must be unique in the region
-- that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must /not/
-- be the literal string @arn@.
--
-- 'workflowExecutionRetentionPeriodInDays', 'registerDomain_workflowExecutionRetentionPeriodInDays' - The duration (in days) that records and histories of workflow executions
-- on the domain should be kept by the service. After the retention period,
-- the workflow execution isn\'t available in the results of visibility
-- calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution
-- history isn\'t retained. As soon as the workflow execution completes,
-- the execution record and its history are deleted.
--
-- The maximum workflow execution retention period is 90 days. For more
-- information about Amazon SWF service limits, see:
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits>
-- in the /Amazon SWF Developer Guide/.
newRegisterDomain ::
  -- | 'name'
  Prelude.Text ->
  -- | 'workflowExecutionRetentionPeriodInDays'
  Prelude.Text ->
  RegisterDomain
newRegisterDomain
  pName_
  pWorkflowExecutionRetentionPeriodInDays_ =
    RegisterDomain'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        workflowExecutionRetentionPeriodInDays =
          pWorkflowExecutionRetentionPeriodInDays_
      }

-- | A text description of the domain.
registerDomain_description :: Lens.Lens' RegisterDomain (Prelude.Maybe Prelude.Text)
registerDomain_description = Lens.lens (\RegisterDomain' {description} -> description) (\s@RegisterDomain' {} a -> s {description = a} :: RegisterDomain)

-- | Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
registerDomain_tags :: Lens.Lens' RegisterDomain (Prelude.Maybe [ResourceTag])
registerDomain_tags = Lens.lens (\RegisterDomain' {tags} -> tags) (\s@RegisterDomain' {} a -> s {tags = a} :: RegisterDomain) Prelude.. Lens.mapping Lens.coerced

-- | Name of the domain to register. The name must be unique in the region
-- that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must /not/
-- be the literal string @arn@.
registerDomain_name :: Lens.Lens' RegisterDomain Prelude.Text
registerDomain_name = Lens.lens (\RegisterDomain' {name} -> name) (\s@RegisterDomain' {} a -> s {name = a} :: RegisterDomain)

-- | The duration (in days) that records and histories of workflow executions
-- on the domain should be kept by the service. After the retention period,
-- the workflow execution isn\'t available in the results of visibility
-- calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution
-- history isn\'t retained. As soon as the workflow execution completes,
-- the execution record and its history are deleted.
--
-- The maximum workflow execution retention period is 90 days. For more
-- information about Amazon SWF service limits, see:
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits>
-- in the /Amazon SWF Developer Guide/.
registerDomain_workflowExecutionRetentionPeriodInDays :: Lens.Lens' RegisterDomain Prelude.Text
registerDomain_workflowExecutionRetentionPeriodInDays = Lens.lens (\RegisterDomain' {workflowExecutionRetentionPeriodInDays} -> workflowExecutionRetentionPeriodInDays) (\s@RegisterDomain' {} a -> s {workflowExecutionRetentionPeriodInDays = a} :: RegisterDomain)

instance Core.AWSRequest RegisterDomain where
  type
    AWSResponse RegisterDomain =
      RegisterDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RegisterDomainResponse'

instance Prelude.Hashable RegisterDomain where
  hashWithSalt _salt RegisterDomain' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` workflowExecutionRetentionPeriodInDays

instance Prelude.NFData RegisterDomain where
  rnf RegisterDomain' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowExecutionRetentionPeriodInDays

instance Data.ToHeaders RegisterDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.RegisterDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterDomain where
  toJSON RegisterDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "workflowExecutionRetentionPeriodInDays"
                  Data..= workflowExecutionRetentionPeriodInDays
              )
          ]
      )

instance Data.ToPath RegisterDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterDomainResponse ::
  RegisterDomainResponse
newRegisterDomainResponse = RegisterDomainResponse'

instance Prelude.NFData RegisterDomainResponse where
  rnf _ = ()
