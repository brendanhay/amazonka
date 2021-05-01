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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTopicRuleDestination' smart constructor.
data GetTopicRuleDestination = GetTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetTopicRuleDestination
newGetTopicRuleDestination pArn_ =
  GetTopicRuleDestination' {arn = pArn_}

-- | The ARN of the topic rule destination.
getTopicRuleDestination_arn :: Lens.Lens' GetTopicRuleDestination Prelude.Text
getTopicRuleDestination_arn = Lens.lens (\GetTopicRuleDestination' {arn} -> arn) (\s@GetTopicRuleDestination' {} a -> s {arn = a} :: GetTopicRuleDestination)

instance Prelude.AWSRequest GetTopicRuleDestination where
  type
    Rs GetTopicRuleDestination =
      GetTopicRuleDestinationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            Prelude.<$> (x Prelude..?> "topicRuleDestination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTopicRuleDestination

instance Prelude.NFData GetTopicRuleDestination

instance Prelude.ToHeaders GetTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetTopicRuleDestination where
  toPath GetTopicRuleDestination' {..} =
    Prelude.mconcat
      ["/destinations/", Prelude.toBS arn]

instance Prelude.ToQuery GetTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Prelude.Maybe TopicRuleDestination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetTopicRuleDestinationResponse
newGetTopicRuleDestinationResponse pHttpStatus_ =
  GetTopicRuleDestinationResponse'
    { topicRuleDestination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The topic rule destination.
getTopicRuleDestinationResponse_topicRuleDestination :: Lens.Lens' GetTopicRuleDestinationResponse (Prelude.Maybe TopicRuleDestination)
getTopicRuleDestinationResponse_topicRuleDestination = Lens.lens (\GetTopicRuleDestinationResponse' {topicRuleDestination} -> topicRuleDestination) (\s@GetTopicRuleDestinationResponse' {} a -> s {topicRuleDestination = a} :: GetTopicRuleDestinationResponse)

-- | The response's http status code.
getTopicRuleDestinationResponse_httpStatus :: Lens.Lens' GetTopicRuleDestinationResponse Prelude.Int
getTopicRuleDestinationResponse_httpStatus = Lens.lens (\GetTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@GetTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: GetTopicRuleDestinationResponse)

instance
  Prelude.NFData
    GetTopicRuleDestinationResponse
