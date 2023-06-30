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
-- Module      : Amazonka.OpsWorks.DescribeAgentVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS OpsWorks Stacks agent versions. You must
-- specify a stack ID or a configuration manager. @DescribeAgentVersions@
-- returns a list of available agent versions for the specified stack or
-- configuration manager.
module Amazonka.OpsWorks.DescribeAgentVersions
  ( -- * Creating a Request
    DescribeAgentVersions (..),
    newDescribeAgentVersions,

    -- * Request Lenses
    describeAgentVersions_configurationManager,
    describeAgentVersions_stackId,

    -- * Destructuring the Response
    DescribeAgentVersionsResponse (..),
    newDescribeAgentVersionsResponse,

    -- * Response Lenses
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAgentVersions' smart constructor.
data DescribeAgentVersions = DescribeAgentVersions'
  { -- | The configuration manager.
    configurationManager :: Prelude.Maybe StackConfigurationManager,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationManager', 'describeAgentVersions_configurationManager' - The configuration manager.
--
-- 'stackId', 'describeAgentVersions_stackId' - The stack ID.
newDescribeAgentVersions ::
  DescribeAgentVersions
newDescribeAgentVersions =
  DescribeAgentVersions'
    { configurationManager =
        Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | The configuration manager.
describeAgentVersions_configurationManager :: Lens.Lens' DescribeAgentVersions (Prelude.Maybe StackConfigurationManager)
describeAgentVersions_configurationManager = Lens.lens (\DescribeAgentVersions' {configurationManager} -> configurationManager) (\s@DescribeAgentVersions' {} a -> s {configurationManager = a} :: DescribeAgentVersions)

-- | The stack ID.
describeAgentVersions_stackId :: Lens.Lens' DescribeAgentVersions (Prelude.Maybe Prelude.Text)
describeAgentVersions_stackId = Lens.lens (\DescribeAgentVersions' {stackId} -> stackId) (\s@DescribeAgentVersions' {} a -> s {stackId = a} :: DescribeAgentVersions)

instance Core.AWSRequest DescribeAgentVersions where
  type
    AWSResponse DescribeAgentVersions =
      DescribeAgentVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentVersionsResponse'
            Prelude.<$> (x Data..?> "AgentVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAgentVersions where
  hashWithSalt _salt DescribeAgentVersions' {..} =
    _salt
      `Prelude.hashWithSalt` configurationManager
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeAgentVersions where
  rnf DescribeAgentVersions' {..} =
    Prelude.rnf configurationManager
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeAgentVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeAgentVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAgentVersions where
  toJSON DescribeAgentVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationManager" Data..=)
              Prelude.<$> configurationManager,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeAgentVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAgentVersions where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeAgentVersions@ request.
--
-- /See:/ 'newDescribeAgentVersionsResponse' smart constructor.
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
  { -- | The agent versions for the specified stack or configuration manager.
    -- Note that this value is the complete version number, not the abbreviated
    -- number used by the console.
    agentVersions :: Prelude.Maybe [AgentVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentVersions', 'describeAgentVersionsResponse_agentVersions' - The agent versions for the specified stack or configuration manager.
-- Note that this value is the complete version number, not the abbreviated
-- number used by the console.
--
-- 'httpStatus', 'describeAgentVersionsResponse_httpStatus' - The response's http status code.
newDescribeAgentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAgentVersionsResponse
newDescribeAgentVersionsResponse pHttpStatus_ =
  DescribeAgentVersionsResponse'
    { agentVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The agent versions for the specified stack or configuration manager.
-- Note that this value is the complete version number, not the abbreviated
-- number used by the console.
describeAgentVersionsResponse_agentVersions :: Lens.Lens' DescribeAgentVersionsResponse (Prelude.Maybe [AgentVersion])
describeAgentVersionsResponse_agentVersions = Lens.lens (\DescribeAgentVersionsResponse' {agentVersions} -> agentVersions) (\s@DescribeAgentVersionsResponse' {} a -> s {agentVersions = a} :: DescribeAgentVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAgentVersionsResponse_httpStatus :: Lens.Lens' DescribeAgentVersionsResponse Prelude.Int
describeAgentVersionsResponse_httpStatus = Lens.lens (\DescribeAgentVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAgentVersionsResponse' {} a -> s {httpStatus = a} :: DescribeAgentVersionsResponse)

instance Prelude.NFData DescribeAgentVersionsResponse where
  rnf DescribeAgentVersionsResponse' {..} =
    Prelude.rnf agentVersions
      `Prelude.seq` Prelude.rnf httpStatus
