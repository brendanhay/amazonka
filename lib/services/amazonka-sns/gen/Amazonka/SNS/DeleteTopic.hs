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
-- Module      : Amazonka.SNS.DeleteTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might
-- prevent some messages previously sent to the topic from being delivered
-- to subscribers. This action is idempotent, so deleting a topic that does
-- not exist does not result in an error.
module Amazonka.SNS.DeleteTopic
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newDeleteTopic' smart constructor.
data DeleteTopic = DeleteTopic'
  { -- | The ARN of the topic you want to delete.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTopic where
  type AWSResponse DeleteTopic = DeleteTopicResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteTopicResponse'

instance Prelude.Hashable DeleteTopic where
  hashWithSalt _salt DeleteTopic' {..} =
    _salt `Prelude.hashWithSalt` topicArn

instance Prelude.NFData DeleteTopic where
  rnf DeleteTopic' {..} = Prelude.rnf topicArn

instance Data.ToHeaders DeleteTopic where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTopic where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTopic where
  toQuery DeleteTopic' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteTopic" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Data.=: topicArn
      ]

-- | /See:/ 'newDeleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTopicResponse ::
  DeleteTopicResponse
newDeleteTopicResponse = DeleteTopicResponse'

instance Prelude.NFData DeleteTopicResponse where
  rnf _ = ()
