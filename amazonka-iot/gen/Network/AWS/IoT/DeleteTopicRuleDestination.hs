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
-- Module      : Network.AWS.IoT.DeleteTopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic rule destination.
module Network.AWS.IoT.DeleteTopicRuleDestination
  ( -- * Creating a Request
    DeleteTopicRuleDestination (..),
    newDeleteTopicRuleDestination,

    -- * Request Lenses
    deleteTopicRuleDestination_arn,

    -- * Destructuring the Response
    DeleteTopicRuleDestinationResponse (..),
    newDeleteTopicRuleDestinationResponse,

    -- * Response Lenses
    deleteTopicRuleDestinationResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTopicRuleDestination' smart constructor.
data DeleteTopicRuleDestination = DeleteTopicRuleDestination'
  { -- | The ARN of the topic rule destination to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteTopicRuleDestination_arn' - The ARN of the topic rule destination to delete.
newDeleteTopicRuleDestination ::
  -- | 'arn'
  Prelude.Text ->
  DeleteTopicRuleDestination
newDeleteTopicRuleDestination pArn_ =
  DeleteTopicRuleDestination' {arn = pArn_}

-- | The ARN of the topic rule destination to delete.
deleteTopicRuleDestination_arn :: Lens.Lens' DeleteTopicRuleDestination Prelude.Text
deleteTopicRuleDestination_arn = Lens.lens (\DeleteTopicRuleDestination' {arn} -> arn) (\s@DeleteTopicRuleDestination' {} a -> s {arn = a} :: DeleteTopicRuleDestination)

instance
  Prelude.AWSRequest
    DeleteTopicRuleDestination
  where
  type
    Rs DeleteTopicRuleDestination =
      DeleteTopicRuleDestinationResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTopicRuleDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTopicRuleDestination

instance Prelude.NFData DeleteTopicRuleDestination

instance Prelude.ToHeaders DeleteTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTopicRuleDestination where
  toPath DeleteTopicRuleDestination' {..} =
    Prelude.mconcat
      ["/destinations/", Prelude.toBS arn]

instance Prelude.ToQuery DeleteTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTopicRuleDestinationResponse' smart constructor.
data DeleteTopicRuleDestinationResponse = DeleteTopicRuleDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRuleDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTopicRuleDestinationResponse_httpStatus' - The response's http status code.
newDeleteTopicRuleDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTopicRuleDestinationResponse
newDeleteTopicRuleDestinationResponse pHttpStatus_ =
  DeleteTopicRuleDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTopicRuleDestinationResponse_httpStatus :: Lens.Lens' DeleteTopicRuleDestinationResponse Prelude.Int
deleteTopicRuleDestinationResponse_httpStatus = Lens.lens (\DeleteTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: DeleteTopicRuleDestinationResponse)

instance
  Prelude.NFData
    DeleteTopicRuleDestinationResponse
