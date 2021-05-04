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
-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might
-- prevent some messages previously sent to the topic from being delivered
-- to subscribers. This action is idempotent, so deleting a topic that does
-- not exist does not result in an error.
module Network.AWS.SNS.DeleteTopic
  ( -- * Creating a Request
    DeleteTopic (..),
    newDeleteTopic,

    -- * Request Lenses
    deleteTopic_topicArn,

    -- * Destructuring the Response
    DeleteTopicResponse (..),
    newDeleteTopicResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | /See:/ 'newDeleteTopic' smart constructor.
data DeleteTopic = DeleteTopic'
  { -- | The ARN of the topic you want to delete.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'deleteTopic_topicArn' - The ARN of the topic you want to delete.
newDeleteTopic ::
  -- | 'topicArn'
  Prelude.Text ->
  DeleteTopic
newDeleteTopic pTopicArn_ =
  DeleteTopic' {topicArn = pTopicArn_}

-- | The ARN of the topic you want to delete.
deleteTopic_topicArn :: Lens.Lens' DeleteTopic Prelude.Text
deleteTopic_topicArn = Lens.lens (\DeleteTopic' {topicArn} -> topicArn) (\s@DeleteTopic' {} a -> s {topicArn = a} :: DeleteTopic)

instance Prelude.AWSRequest DeleteTopic where
  type Rs DeleteTopic = DeleteTopicResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteTopicResponse'

instance Prelude.Hashable DeleteTopic

instance Prelude.NFData DeleteTopic

instance Prelude.ToHeaders DeleteTopic where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTopic where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTopic where
  toQuery DeleteTopic' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteTopic" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Prelude.=: topicArn
      ]

-- | /See:/ 'newDeleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTopicResponse ::
  DeleteTopicResponse
newDeleteTopicResponse = DeleteTopicResponse'

instance Prelude.NFData DeleteTopicResponse
