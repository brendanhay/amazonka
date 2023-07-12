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
-- Module      : Amazonka.Inspector.PreviewAgents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are part of the
-- specified assessment target.
--
-- This operation returns paginated results.
module Amazonka.Inspector.PreviewAgents
  ( -- * Creating a Request
    PreviewAgents (..),
    newPreviewAgents,

    -- * Request Lenses
    previewAgents_maxResults,
    previewAgents_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPreviewAgents' smart constructor.
data PreviewAgents = PreviewAgents'
  { -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the __PreviewAgents__
    -- action. Subsequent calls to the action fill __nextToken__ in the request
    -- with the value of __NextToken__ from the previous response to continue
    -- listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the assessment target whose agents you want to preview.
    previewAgentsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PreviewAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'previewAgents_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'nextToken', 'previewAgents_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __PreviewAgents__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
--
-- 'previewAgentsArn', 'previewAgents_previewAgentsArn' - The ARN of the assessment target whose agents you want to preview.
newPreviewAgents ::
  -- | 'previewAgentsArn'
  Prelude.Text ->
  PreviewAgents
newPreviewAgents pPreviewAgentsArn_ =
  PreviewAgents'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      previewAgentsArn = pPreviewAgentsArn_
    }

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
previewAgents_maxResults :: Lens.Lens' PreviewAgents (Prelude.Maybe Prelude.Int)
previewAgents_maxResults = Lens.lens (\PreviewAgents' {maxResults} -> maxResults) (\s@PreviewAgents' {} a -> s {maxResults = a} :: PreviewAgents)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __PreviewAgents__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
previewAgents_nextToken :: Lens.Lens' PreviewAgents (Prelude.Maybe Prelude.Text)
previewAgents_nextToken = Lens.lens (\PreviewAgents' {nextToken} -> nextToken) (\s@PreviewAgents' {} a -> s {nextToken = a} :: PreviewAgents)

-- | The ARN of the assessment target whose agents you want to preview.
previewAgents_previewAgentsArn :: Lens.Lens' PreviewAgents Prelude.Text
previewAgents_previewAgentsArn = Lens.lens (\PreviewAgents' {previewAgentsArn} -> previewAgentsArn) (\s@PreviewAgents' {} a -> s {previewAgentsArn = a} :: PreviewAgents)

instance Core.AWSPager PreviewAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? previewAgentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. previewAgentsResponse_agentPreviews) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& previewAgents_nextToken
          Lens..~ rs
          Lens.^? previewAgentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest PreviewAgents where
  type
    AWSResponse PreviewAgents =
      PreviewAgentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PreviewAgentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "agentPreviews" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable PreviewAgents where
  hashWithSalt _salt PreviewAgents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` previewAgentsArn

instance Prelude.NFData PreviewAgents where
  rnf PreviewAgents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf previewAgentsArn

instance Data.ToHeaders PreviewAgents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.PreviewAgents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PreviewAgents where
  toJSON PreviewAgents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("previewAgentsArn" Data..= previewAgentsArn)
          ]
      )

instance Data.ToPath PreviewAgents where
  toPath = Prelude.const "/"

instance Data.ToQuery PreviewAgents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPreviewAgentsResponse' smart constructor.
data PreviewAgentsResponse = PreviewAgentsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resulting list of agents.
    agentPreviews :: [AgentPreview]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PreviewAgentsResponse
newPreviewAgentsResponse pHttpStatus_ =
  PreviewAgentsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      agentPreviews = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
previewAgentsResponse_nextToken :: Lens.Lens' PreviewAgentsResponse (Prelude.Maybe Prelude.Text)
previewAgentsResponse_nextToken = Lens.lens (\PreviewAgentsResponse' {nextToken} -> nextToken) (\s@PreviewAgentsResponse' {} a -> s {nextToken = a} :: PreviewAgentsResponse)

-- | The response's http status code.
previewAgentsResponse_httpStatus :: Lens.Lens' PreviewAgentsResponse Prelude.Int
previewAgentsResponse_httpStatus = Lens.lens (\PreviewAgentsResponse' {httpStatus} -> httpStatus) (\s@PreviewAgentsResponse' {} a -> s {httpStatus = a} :: PreviewAgentsResponse)

-- | The resulting list of agents.
previewAgentsResponse_agentPreviews :: Lens.Lens' PreviewAgentsResponse [AgentPreview]
previewAgentsResponse_agentPreviews = Lens.lens (\PreviewAgentsResponse' {agentPreviews} -> agentPreviews) (\s@PreviewAgentsResponse' {} a -> s {agentPreviews = a} :: PreviewAgentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData PreviewAgentsResponse where
  rnf PreviewAgentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf agentPreviews
