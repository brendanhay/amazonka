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
-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic\'s owner can
-- unsubscribe, and an AWS signature is required. If the @Unsubscribe@ call
-- does not require authentication and the requester is not the
-- subscription owner, a final cancellation message is delivered to the
-- endpoint, so that the endpoint owner can easily resubscribe to the topic
-- if the @Unsubscribe@ request was unintended.
--
-- This action is throttled at 100 transactions per second (TPS).
module Network.AWS.SNS.Unsubscribe
  ( -- * Creating a Request
    Unsubscribe (..),
    newUnsubscribe,

    -- * Request Lenses
    unsubscribe_subscriptionArn,

    -- * Destructuring the Response
    UnsubscribeResponse (..),
    newUnsubscribeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for Unsubscribe action.
--
-- /See:/ 'newUnsubscribe' smart constructor.
data Unsubscribe = Unsubscribe'
  { -- | The ARN of the subscription to be deleted.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Unsubscribe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionArn', 'unsubscribe_subscriptionArn' - The ARN of the subscription to be deleted.
newUnsubscribe ::
  -- | 'subscriptionArn'
  Prelude.Text ->
  Unsubscribe
newUnsubscribe pSubscriptionArn_ =
  Unsubscribe' {subscriptionArn = pSubscriptionArn_}

-- | The ARN of the subscription to be deleted.
unsubscribe_subscriptionArn :: Lens.Lens' Unsubscribe Prelude.Text
unsubscribe_subscriptionArn = Lens.lens (\Unsubscribe' {subscriptionArn} -> subscriptionArn) (\s@Unsubscribe' {} a -> s {subscriptionArn = a} :: Unsubscribe)

instance Prelude.AWSRequest Unsubscribe where
  type Rs Unsubscribe = UnsubscribeResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull UnsubscribeResponse'

instance Prelude.Hashable Unsubscribe

instance Prelude.NFData Unsubscribe

instance Prelude.ToHeaders Unsubscribe where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath Unsubscribe where
  toPath = Prelude.const "/"

instance Prelude.ToQuery Unsubscribe where
  toQuery Unsubscribe' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("Unsubscribe" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "SubscriptionArn" Prelude.=: subscriptionArn
      ]

-- | /See:/ 'newUnsubscribeResponse' smart constructor.
data UnsubscribeResponse = UnsubscribeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnsubscribeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnsubscribeResponse ::
  UnsubscribeResponse
newUnsubscribeResponse = UnsubscribeResponse'

instance Prelude.NFData UnsubscribeResponse
