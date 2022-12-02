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
-- Module      : Amazonka.AMP.DescribeRuleGroupsNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a rule groups namespace.
module Amazonka.AMP.DescribeRuleGroupsNamespace
  ( -- * Creating a Request
    DescribeRuleGroupsNamespace (..),
    newDescribeRuleGroupsNamespace,

    -- * Request Lenses
    describeRuleGroupsNamespace_name,
    describeRuleGroupsNamespace_workspaceId,

    -- * Destructuring the Response
    DescribeRuleGroupsNamespaceResponse (..),
    newDescribeRuleGroupsNamespaceResponse,

    -- * Response Lenses
    describeRuleGroupsNamespaceResponse_httpStatus,
    describeRuleGroupsNamespaceResponse_ruleGroupsNamespace,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a DescribeRuleGroupsNamespace operation.
--
-- /See:/ 'newDescribeRuleGroupsNamespace' smart constructor.
data DescribeRuleGroupsNamespace = DescribeRuleGroupsNamespace'
  { -- | The rule groups namespace.
    name :: Prelude.Text,
    -- | The ID of the workspace to describe.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroupsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeRuleGroupsNamespace_name' - The rule groups namespace.
--
-- 'workspaceId', 'describeRuleGroupsNamespace_workspaceId' - The ID of the workspace to describe.
newDescribeRuleGroupsNamespace ::
  -- | 'name'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  DescribeRuleGroupsNamespace
newDescribeRuleGroupsNamespace pName_ pWorkspaceId_ =
  DescribeRuleGroupsNamespace'
    { name = pName_,
      workspaceId = pWorkspaceId_
    }

-- | The rule groups namespace.
describeRuleGroupsNamespace_name :: Lens.Lens' DescribeRuleGroupsNamespace Prelude.Text
describeRuleGroupsNamespace_name = Lens.lens (\DescribeRuleGroupsNamespace' {name} -> name) (\s@DescribeRuleGroupsNamespace' {} a -> s {name = a} :: DescribeRuleGroupsNamespace)

-- | The ID of the workspace to describe.
describeRuleGroupsNamespace_workspaceId :: Lens.Lens' DescribeRuleGroupsNamespace Prelude.Text
describeRuleGroupsNamespace_workspaceId = Lens.lens (\DescribeRuleGroupsNamespace' {workspaceId} -> workspaceId) (\s@DescribeRuleGroupsNamespace' {} a -> s {workspaceId = a} :: DescribeRuleGroupsNamespace)

instance Core.AWSRequest DescribeRuleGroupsNamespace where
  type
    AWSResponse DescribeRuleGroupsNamespace =
      DescribeRuleGroupsNamespaceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleGroupsNamespaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ruleGroupsNamespace")
      )

instance Prelude.Hashable DescribeRuleGroupsNamespace where
  hashWithSalt _salt DescribeRuleGroupsNamespace' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DescribeRuleGroupsNamespace where
  rnf DescribeRuleGroupsNamespace' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders DescribeRuleGroupsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRuleGroupsNamespace where
  toPath DescribeRuleGroupsNamespace' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/rulegroupsnamespaces/",
        Data.toBS name
      ]

instance Data.ToQuery DescribeRuleGroupsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a DescribeRuleGroupsNamespace operation.
--
-- /See:/ 'newDescribeRuleGroupsNamespaceResponse' smart constructor.
data DescribeRuleGroupsNamespaceResponse = DescribeRuleGroupsNamespaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The selected rule groups namespace.
    ruleGroupsNamespace :: RuleGroupsNamespaceDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroupsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeRuleGroupsNamespaceResponse_httpStatus' - The response's http status code.
--
-- 'ruleGroupsNamespace', 'describeRuleGroupsNamespaceResponse_ruleGroupsNamespace' - The selected rule groups namespace.
newDescribeRuleGroupsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'ruleGroupsNamespace'
  RuleGroupsNamespaceDescription ->
  DescribeRuleGroupsNamespaceResponse
newDescribeRuleGroupsNamespaceResponse
  pHttpStatus_
  pRuleGroupsNamespace_ =
    DescribeRuleGroupsNamespaceResponse'
      { httpStatus =
          pHttpStatus_,
        ruleGroupsNamespace =
          pRuleGroupsNamespace_
      }

-- | The response's http status code.
describeRuleGroupsNamespaceResponse_httpStatus :: Lens.Lens' DescribeRuleGroupsNamespaceResponse Prelude.Int
describeRuleGroupsNamespaceResponse_httpStatus = Lens.lens (\DescribeRuleGroupsNamespaceResponse' {httpStatus} -> httpStatus) (\s@DescribeRuleGroupsNamespaceResponse' {} a -> s {httpStatus = a} :: DescribeRuleGroupsNamespaceResponse)

-- | The selected rule groups namespace.
describeRuleGroupsNamespaceResponse_ruleGroupsNamespace :: Lens.Lens' DescribeRuleGroupsNamespaceResponse RuleGroupsNamespaceDescription
describeRuleGroupsNamespaceResponse_ruleGroupsNamespace = Lens.lens (\DescribeRuleGroupsNamespaceResponse' {ruleGroupsNamespace} -> ruleGroupsNamespace) (\s@DescribeRuleGroupsNamespaceResponse' {} a -> s {ruleGroupsNamespace = a} :: DescribeRuleGroupsNamespaceResponse)

instance
  Prelude.NFData
    DescribeRuleGroupsNamespaceResponse
  where
  rnf DescribeRuleGroupsNamespaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleGroupsNamespace
