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
-- Module      : Amazonka.IoT.GetTopicRuleDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a topic rule destination.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetTopicRuleDestination>
-- action.
module Amazonka.IoT.GetTopicRuleDestination
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTopicRuleDestination' smart constructor.
data GetTopicRuleDestination = GetTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetTopicRuleDestination where
  type
    AWSResponse GetTopicRuleDestination =
      GetTopicRuleDestinationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            Prelude.<$> (x Data..?> "topicRuleDestination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTopicRuleDestination where
  hashWithSalt _salt GetTopicRuleDestination' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetTopicRuleDestination where
  rnf GetTopicRuleDestination' {..} = Prelude.rnf arn

instance Data.ToHeaders GetTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTopicRuleDestination where
  toPath GetTopicRuleDestination' {..} =
    Prelude.mconcat ["/destinations/", Data.toBS arn]

instance Data.ToQuery GetTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Prelude.Maybe TopicRuleDestination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetTopicRuleDestinationResponse' {..} =
    Prelude.rnf topicRuleDestination
      `Prelude.seq` Prelude.rnf httpStatus
