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
-- Module      : Network.AWS.IoT.UpdateTopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a topic rule destination. You use this to change the status,
-- endpoint URL, or confirmation URL of the destination.
module Network.AWS.IoT.UpdateTopicRuleDestination
  ( -- * Creating a Request
    UpdateTopicRuleDestination (..),
    newUpdateTopicRuleDestination,

    -- * Request Lenses
    updateTopicRuleDestination_arn,
    updateTopicRuleDestination_status,

    -- * Destructuring the Response
    UpdateTopicRuleDestinationResponse (..),
    newUpdateTopicRuleDestinationResponse,

    -- * Response Lenses
    updateTopicRuleDestinationResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTopicRuleDestination' smart constructor.
data UpdateTopicRuleDestination = UpdateTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Prelude.Text,
    -- | The status of the topic rule destination. Valid values are:
    --
    -- [IN_PROGRESS]
    --     A topic rule destination was created but has not been confirmed. You
    --     can set @status@ to @IN_PROGRESS@ by calling
    --     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
    --     causes a new confirmation challenge to be sent to your confirmation
    --     endpoint.
    --
    -- [ENABLED]
    --     Confirmation was completed, and traffic to this destination is
    --     allowed. You can set @status@ to @DISABLED@ by calling
    --     @UpdateTopicRuleDestination@.
    --
    -- [DISABLED]
    --     Confirmation was completed, and traffic to this destination is not
    --     allowed. You can set @status@ to @ENABLED@ by calling
    --     @UpdateTopicRuleDestination@.
    --
    -- [ERROR]
    --     Confirmation could not be completed, for example if the confirmation
    --     timed out. You can call @GetTopicRuleDestination@ for details about
    --     the error. You can set @status@ to @IN_PROGRESS@ by calling
    --     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
    --     causes a new confirmation challenge to be sent to your confirmation
    --     endpoint.
    status :: TopicRuleDestinationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateTopicRuleDestination_arn' - The ARN of the topic rule destination.
--
-- 'status', 'updateTopicRuleDestination_status' - The status of the topic rule destination. Valid values are:
--
-- [IN_PROGRESS]
--     A topic rule destination was created but has not been confirmed. You
--     can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
--
-- [ENABLED]
--     Confirmation was completed, and traffic to this destination is
--     allowed. You can set @status@ to @DISABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [DISABLED]
--     Confirmation was completed, and traffic to this destination is not
--     allowed. You can set @status@ to @ENABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [ERROR]
--     Confirmation could not be completed, for example if the confirmation
--     timed out. You can call @GetTopicRuleDestination@ for details about
--     the error. You can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
newUpdateTopicRuleDestination ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  TopicRuleDestinationStatus ->
  UpdateTopicRuleDestination
newUpdateTopicRuleDestination pArn_ pStatus_ =
  UpdateTopicRuleDestination'
    { arn = pArn_,
      status = pStatus_
    }

-- | The ARN of the topic rule destination.
updateTopicRuleDestination_arn :: Lens.Lens' UpdateTopicRuleDestination Prelude.Text
updateTopicRuleDestination_arn = Lens.lens (\UpdateTopicRuleDestination' {arn} -> arn) (\s@UpdateTopicRuleDestination' {} a -> s {arn = a} :: UpdateTopicRuleDestination)

-- | The status of the topic rule destination. Valid values are:
--
-- [IN_PROGRESS]
--     A topic rule destination was created but has not been confirmed. You
--     can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
--
-- [ENABLED]
--     Confirmation was completed, and traffic to this destination is
--     allowed. You can set @status@ to @DISABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [DISABLED]
--     Confirmation was completed, and traffic to this destination is not
--     allowed. You can set @status@ to @ENABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [ERROR]
--     Confirmation could not be completed, for example if the confirmation
--     timed out. You can call @GetTopicRuleDestination@ for details about
--     the error. You can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
updateTopicRuleDestination_status :: Lens.Lens' UpdateTopicRuleDestination TopicRuleDestinationStatus
updateTopicRuleDestination_status = Lens.lens (\UpdateTopicRuleDestination' {status} -> status) (\s@UpdateTopicRuleDestination' {} a -> s {status = a} :: UpdateTopicRuleDestination)

instance
  Prelude.AWSRequest
    UpdateTopicRuleDestination
  where
  type
    Rs UpdateTopicRuleDestination =
      UpdateTopicRuleDestinationResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTopicRuleDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTopicRuleDestination

instance Prelude.NFData UpdateTopicRuleDestination

instance Prelude.ToHeaders UpdateTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateTopicRuleDestination where
  toJSON UpdateTopicRuleDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("arn" Prelude..= arn),
            Prelude.Just ("status" Prelude..= status)
          ]
      )

instance Prelude.ToPath UpdateTopicRuleDestination where
  toPath = Prelude.const "/destinations"

instance Prelude.ToQuery UpdateTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTopicRuleDestinationResponse' smart constructor.
data UpdateTopicRuleDestinationResponse = UpdateTopicRuleDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicRuleDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTopicRuleDestinationResponse_httpStatus' - The response's http status code.
newUpdateTopicRuleDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTopicRuleDestinationResponse
newUpdateTopicRuleDestinationResponse pHttpStatus_ =
  UpdateTopicRuleDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTopicRuleDestinationResponse_httpStatus :: Lens.Lens' UpdateTopicRuleDestinationResponse Prelude.Int
updateTopicRuleDestinationResponse_httpStatus = Lens.lens (\UpdateTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: UpdateTopicRuleDestinationResponse)

instance
  Prelude.NFData
    UpdateTopicRuleDestinationResponse
