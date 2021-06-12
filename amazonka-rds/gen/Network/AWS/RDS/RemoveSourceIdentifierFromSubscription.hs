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
-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a source identifier from an existing RDS event notification
-- subscription.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
  ( -- * Creating a Request
    RemoveSourceIdentifierFromSubscription (..),
    newRemoveSourceIdentifierFromSubscription,

    -- * Request Lenses
    removeSourceIdentifierFromSubscription_subscriptionName,
    removeSourceIdentifierFromSubscription_sourceIdentifier,

    -- * Destructuring the Response
    RemoveSourceIdentifierFromSubscriptionResponse (..),
    newRemoveSourceIdentifierFromSubscriptionResponse,

    -- * Response Lenses
    removeSourceIdentifierFromSubscriptionResponse_eventSubscription,
    removeSourceIdentifierFromSubscriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRemoveSourceIdentifierFromSubscription' smart constructor.
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription'
  { -- | The name of the RDS event notification subscription you want to remove a
    -- source identifier from.
    subscriptionName :: Core.Text,
    -- | The source identifier to be removed from the subscription, such as the
    -- __DB instance identifier__ for a DB instance or the name of a security
    -- group.
    sourceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveSourceIdentifierFromSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionName', 'removeSourceIdentifierFromSubscription_subscriptionName' - The name of the RDS event notification subscription you want to remove a
-- source identifier from.
--
-- 'sourceIdentifier', 'removeSourceIdentifierFromSubscription_sourceIdentifier' - The source identifier to be removed from the subscription, such as the
-- __DB instance identifier__ for a DB instance or the name of a security
-- group.
newRemoveSourceIdentifierFromSubscription ::
  -- | 'subscriptionName'
  Core.Text ->
  -- | 'sourceIdentifier'
  Core.Text ->
  RemoveSourceIdentifierFromSubscription
newRemoveSourceIdentifierFromSubscription
  pSubscriptionName_
  pSourceIdentifier_ =
    RemoveSourceIdentifierFromSubscription'
      { subscriptionName =
          pSubscriptionName_,
        sourceIdentifier =
          pSourceIdentifier_
      }

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
removeSourceIdentifierFromSubscription_subscriptionName :: Lens.Lens' RemoveSourceIdentifierFromSubscription Core.Text
removeSourceIdentifierFromSubscription_subscriptionName = Lens.lens (\RemoveSourceIdentifierFromSubscription' {subscriptionName} -> subscriptionName) (\s@RemoveSourceIdentifierFromSubscription' {} a -> s {subscriptionName = a} :: RemoveSourceIdentifierFromSubscription)

-- | The source identifier to be removed from the subscription, such as the
-- __DB instance identifier__ for a DB instance or the name of a security
-- group.
removeSourceIdentifierFromSubscription_sourceIdentifier :: Lens.Lens' RemoveSourceIdentifierFromSubscription Core.Text
removeSourceIdentifierFromSubscription_sourceIdentifier = Lens.lens (\RemoveSourceIdentifierFromSubscription' {sourceIdentifier} -> sourceIdentifier) (\s@RemoveSourceIdentifierFromSubscription' {} a -> s {sourceIdentifier = a} :: RemoveSourceIdentifierFromSubscription)

instance
  Core.AWSRequest
    RemoveSourceIdentifierFromSubscription
  where
  type
    AWSResponse
      RemoveSourceIdentifierFromSubscription =
      RemoveSourceIdentifierFromSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RemoveSourceIdentifierFromSubscriptionResult"
      ( \s h x ->
          RemoveSourceIdentifierFromSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RemoveSourceIdentifierFromSubscription

instance
  Core.NFData
    RemoveSourceIdentifierFromSubscription

instance
  Core.ToHeaders
    RemoveSourceIdentifierFromSubscription
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RemoveSourceIdentifierFromSubscription
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RemoveSourceIdentifierFromSubscription
  where
  toQuery RemoveSourceIdentifierFromSubscription' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RemoveSourceIdentifierFromSubscription" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "SubscriptionName" Core.=: subscriptionName,
        "SourceIdentifier" Core.=: sourceIdentifier
      ]

-- | /See:/ 'newRemoveSourceIdentifierFromSubscriptionResponse' smart constructor.
data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse'
  { eventSubscription :: Core.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveSourceIdentifierFromSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscription', 'removeSourceIdentifierFromSubscriptionResponse_eventSubscription' - Undocumented member.
--
-- 'httpStatus', 'removeSourceIdentifierFromSubscriptionResponse_httpStatus' - The response's http status code.
newRemoveSourceIdentifierFromSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveSourceIdentifierFromSubscriptionResponse
newRemoveSourceIdentifierFromSubscriptionResponse
  pHttpStatus_ =
    RemoveSourceIdentifierFromSubscriptionResponse'
      { eventSubscription =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
removeSourceIdentifierFromSubscriptionResponse_eventSubscription :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse (Core.Maybe EventSubscription)
removeSourceIdentifierFromSubscriptionResponse_eventSubscription = Lens.lens (\RemoveSourceIdentifierFromSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@RemoveSourceIdentifierFromSubscriptionResponse' {} a -> s {eventSubscription = a} :: RemoveSourceIdentifierFromSubscriptionResponse)

-- | The response's http status code.
removeSourceIdentifierFromSubscriptionResponse_httpStatus :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse Core.Int
removeSourceIdentifierFromSubscriptionResponse_httpStatus = Lens.lens (\RemoveSourceIdentifierFromSubscriptionResponse' {httpStatus} -> httpStatus) (\s@RemoveSourceIdentifierFromSubscriptionResponse' {} a -> s {httpStatus = a} :: RemoveSourceIdentifierFromSubscriptionResponse)

instance
  Core.NFData
    RemoveSourceIdentifierFromSubscriptionResponse
