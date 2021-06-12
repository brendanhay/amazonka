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
-- Module      : Network.AWS.OpsWorks.DescribeAgentVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS OpsWorks Stacks agent versions. You must
-- specify a stack ID or a configuration manager. @DescribeAgentVersions@
-- returns a list of available agent versions for the specified stack or
-- configuration manager.
module Network.AWS.OpsWorks.DescribeAgentVersions
  ( -- * Creating a Request
    DescribeAgentVersions (..),
    newDescribeAgentVersions,

    -- * Request Lenses
    describeAgentVersions_stackId,
    describeAgentVersions_configurationManager,

    -- * Destructuring the Response
    DescribeAgentVersionsResponse (..),
    newDescribeAgentVersionsResponse,

    -- * Response Lenses
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAgentVersions' smart constructor.
data DescribeAgentVersions = DescribeAgentVersions'
  { -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | The configuration manager.
    configurationManager :: Core.Maybe StackConfigurationManager
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAgentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeAgentVersions_stackId' - The stack ID.
--
-- 'configurationManager', 'describeAgentVersions_configurationManager' - The configuration manager.
newDescribeAgentVersions ::
  DescribeAgentVersions
newDescribeAgentVersions =
  DescribeAgentVersions'
    { stackId = Core.Nothing,
      configurationManager = Core.Nothing
    }

-- | The stack ID.
describeAgentVersions_stackId :: Lens.Lens' DescribeAgentVersions (Core.Maybe Core.Text)
describeAgentVersions_stackId = Lens.lens (\DescribeAgentVersions' {stackId} -> stackId) (\s@DescribeAgentVersions' {} a -> s {stackId = a} :: DescribeAgentVersions)

-- | The configuration manager.
describeAgentVersions_configurationManager :: Lens.Lens' DescribeAgentVersions (Core.Maybe StackConfigurationManager)
describeAgentVersions_configurationManager = Lens.lens (\DescribeAgentVersions' {configurationManager} -> configurationManager) (\s@DescribeAgentVersions' {} a -> s {configurationManager = a} :: DescribeAgentVersions)

instance Core.AWSRequest DescribeAgentVersions where
  type
    AWSResponse DescribeAgentVersions =
      DescribeAgentVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentVersionsResponse'
            Core.<$> (x Core..?> "AgentVersions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAgentVersions

instance Core.NFData DescribeAgentVersions

instance Core.ToHeaders DescribeAgentVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeAgentVersions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAgentVersions where
  toJSON DescribeAgentVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StackId" Core..=) Core.<$> stackId,
            ("ConfigurationManager" Core..=)
              Core.<$> configurationManager
          ]
      )

instance Core.ToPath DescribeAgentVersions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAgentVersions where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeAgentVersions@ request.
--
-- /See:/ 'newDescribeAgentVersionsResponse' smart constructor.
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
  { -- | The agent versions for the specified stack or configuration manager.
    -- Note that this value is the complete version number, not the abbreviated
    -- number used by the console.
    agentVersions :: Core.Maybe [AgentVersion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAgentVersionsResponse
newDescribeAgentVersionsResponse pHttpStatus_ =
  DescribeAgentVersionsResponse'
    { agentVersions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The agent versions for the specified stack or configuration manager.
-- Note that this value is the complete version number, not the abbreviated
-- number used by the console.
describeAgentVersionsResponse_agentVersions :: Lens.Lens' DescribeAgentVersionsResponse (Core.Maybe [AgentVersion])
describeAgentVersionsResponse_agentVersions = Lens.lens (\DescribeAgentVersionsResponse' {agentVersions} -> agentVersions) (\s@DescribeAgentVersionsResponse' {} a -> s {agentVersions = a} :: DescribeAgentVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAgentVersionsResponse_httpStatus :: Lens.Lens' DescribeAgentVersionsResponse Core.Int
describeAgentVersionsResponse_httpStatus = Lens.lens (\DescribeAgentVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAgentVersionsResponse' {} a -> s {httpStatus = a} :: DescribeAgentVersionsResponse)

instance Core.NFData DescribeAgentVersionsResponse
