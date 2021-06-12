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
-- Module      : Network.AWS.IoT.GetTopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a topic rule destination.
module Network.AWS.IoT.GetTopicRuleDestination
  ( -- * Creating a Request
    GetTopicRuleDestination (..),
    newGetTopicRuleDestination,

    -- * Request Lenses
    getTopicRuleDestination_arn,

    -- * Destructuring the Response
    GetTopicRuleDestinationResponse (..),
    newGetTopicRuleDestinationResponse,

    -- * Response Lenses
    getTopicRuleDestinationResponse_topicRuleDestination,
    getTopicRuleDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTopicRuleDestination' smart constructor.
data GetTopicRuleDestination = GetTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTopicRuleDestination_arn' - The ARN of the topic rule destination.
newGetTopicRuleDestination ::
  -- | 'arn'
  Core.Text ->
  GetTopicRuleDestination
newGetTopicRuleDestination pArn_ =
  GetTopicRuleDestination' {arn = pArn_}

-- | The ARN of the topic rule destination.
getTopicRuleDestination_arn :: Lens.Lens' GetTopicRuleDestination Core.Text
getTopicRuleDestination_arn = Lens.lens (\GetTopicRuleDestination' {arn} -> arn) (\s@GetTopicRuleDestination' {} a -> s {arn = a} :: GetTopicRuleDestination)

instance Core.AWSRequest GetTopicRuleDestination where
  type
    AWSResponse GetTopicRuleDestination =
      GetTopicRuleDestinationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            Core.<$> (x Core..?> "topicRuleDestination")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTopicRuleDestination

instance Core.NFData GetTopicRuleDestination

instance Core.ToHeaders GetTopicRuleDestination where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetTopicRuleDestination where
  toPath GetTopicRuleDestination' {..} =
    Core.mconcat ["/destinations/", Core.toBS arn]

instance Core.ToQuery GetTopicRuleDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Core.Maybe TopicRuleDestination,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTopicRuleDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicRuleDestination', 'getTopicRuleDestinationResponse_topicRuleDestination' - The topic rule destination.
--
-- 'httpStatus', 'getTopicRuleDestinationResponse_httpStatus' - The response's http status code.
newGetTopicRuleDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTopicRuleDestinationResponse
newGetTopicRuleDestinationResponse pHttpStatus_ =
  GetTopicRuleDestinationResponse'
    { topicRuleDestination =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The topic rule destination.
getTopicRuleDestinationResponse_topicRuleDestination :: Lens.Lens' GetTopicRuleDestinationResponse (Core.Maybe TopicRuleDestination)
getTopicRuleDestinationResponse_topicRuleDestination = Lens.lens (\GetTopicRuleDestinationResponse' {topicRuleDestination} -> topicRuleDestination) (\s@GetTopicRuleDestinationResponse' {} a -> s {topicRuleDestination = a} :: GetTopicRuleDestinationResponse)

-- | The response's http status code.
getTopicRuleDestinationResponse_httpStatus :: Lens.Lens' GetTopicRuleDestinationResponse Core.Int
getTopicRuleDestinationResponse_httpStatus = Lens.lens (\GetTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@GetTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: GetTopicRuleDestinationResponse)

instance Core.NFData GetTopicRuleDestinationResponse
