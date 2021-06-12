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
-- Module      : Network.AWS.Inspector.PreviewAgents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are part of the
-- specified assessment target.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.PreviewAgents
  ( -- * Creating a Request
    PreviewAgents (..),
    newPreviewAgents,

    -- * Request Lenses
    previewAgents_nextToken,
    previewAgents_maxResults,
    previewAgents_previewAgentsArn,

    -- * Destructuring the Response
    PreviewAgentsResponse (..),
    newPreviewAgentsResponse,

    -- * Response Lenses
    previewAgentsResponse_nextToken,
    previewAgentsResponse_httpStatus,
    previewAgentsResponse_agentPreviews,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPreviewAgents' smart constructor.
data PreviewAgents = PreviewAgents'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the __PreviewAgents__
    -- action. Subsequent calls to the action fill __nextToken__ in the request
    -- with the value of __NextToken__ from the previous response to continue
    -- listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Core.Maybe Core.Int,
    -- | The ARN of the assessment target whose agents you want to preview.
    previewAgentsArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PreviewAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'previewAgents_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __PreviewAgents__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
--
-- 'maxResults', 'previewAgents_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'previewAgentsArn', 'previewAgents_previewAgentsArn' - The ARN of the assessment target whose agents you want to preview.
newPreviewAgents ::
  -- | 'previewAgentsArn'
  Core.Text ->
  PreviewAgents
newPreviewAgents pPreviewAgentsArn_ =
  PreviewAgents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      previewAgentsArn = pPreviewAgentsArn_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __PreviewAgents__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
previewAgents_nextToken :: Lens.Lens' PreviewAgents (Core.Maybe Core.Text)
previewAgents_nextToken = Lens.lens (\PreviewAgents' {nextToken} -> nextToken) (\s@PreviewAgents' {} a -> s {nextToken = a} :: PreviewAgents)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
previewAgents_maxResults :: Lens.Lens' PreviewAgents (Core.Maybe Core.Int)
previewAgents_maxResults = Lens.lens (\PreviewAgents' {maxResults} -> maxResults) (\s@PreviewAgents' {} a -> s {maxResults = a} :: PreviewAgents)

-- | The ARN of the assessment target whose agents you want to preview.
previewAgents_previewAgentsArn :: Lens.Lens' PreviewAgents Core.Text
previewAgents_previewAgentsArn = Lens.lens (\PreviewAgents' {previewAgentsArn} -> previewAgentsArn) (\s@PreviewAgents' {} a -> s {previewAgentsArn = a} :: PreviewAgents)

instance Core.AWSPager PreviewAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? previewAgentsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. previewAgentsResponse_agentPreviews) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& previewAgents_nextToken
          Lens..~ rs
          Lens.^? previewAgentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest PreviewAgents where
  type
    AWSResponse PreviewAgents =
      PreviewAgentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PreviewAgentsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "agentPreviews" Core..!@ Core.mempty)
      )

instance Core.Hashable PreviewAgents

instance Core.NFData PreviewAgents

instance Core.ToHeaders PreviewAgents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.PreviewAgents" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PreviewAgents where
  toJSON PreviewAgents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("previewAgentsArn" Core..= previewAgentsArn)
          ]
      )

instance Core.ToPath PreviewAgents where
  toPath = Core.const "/"

instance Core.ToQuery PreviewAgents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPreviewAgentsResponse' smart constructor.
data PreviewAgentsResponse = PreviewAgentsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The resulting list of agents.
    agentPreviews :: [AgentPreview]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PreviewAgentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'previewAgentsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'previewAgentsResponse_httpStatus' - The response's http status code.
--
-- 'agentPreviews', 'previewAgentsResponse_agentPreviews' - The resulting list of agents.
newPreviewAgentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PreviewAgentsResponse
newPreviewAgentsResponse pHttpStatus_ =
  PreviewAgentsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      agentPreviews = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
previewAgentsResponse_nextToken :: Lens.Lens' PreviewAgentsResponse (Core.Maybe Core.Text)
previewAgentsResponse_nextToken = Lens.lens (\PreviewAgentsResponse' {nextToken} -> nextToken) (\s@PreviewAgentsResponse' {} a -> s {nextToken = a} :: PreviewAgentsResponse)

-- | The response's http status code.
previewAgentsResponse_httpStatus :: Lens.Lens' PreviewAgentsResponse Core.Int
previewAgentsResponse_httpStatus = Lens.lens (\PreviewAgentsResponse' {httpStatus} -> httpStatus) (\s@PreviewAgentsResponse' {} a -> s {httpStatus = a} :: PreviewAgentsResponse)

-- | The resulting list of agents.
previewAgentsResponse_agentPreviews :: Lens.Lens' PreviewAgentsResponse [AgentPreview]
previewAgentsResponse_agentPreviews = Lens.lens (\PreviewAgentsResponse' {agentPreviews} -> agentPreviews) (\s@PreviewAgentsResponse' {} a -> s {agentPreviews = a} :: PreviewAgentsResponse) Core.. Lens._Coerce

instance Core.NFData PreviewAgentsResponse
