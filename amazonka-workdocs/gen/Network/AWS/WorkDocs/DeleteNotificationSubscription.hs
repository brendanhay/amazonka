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
-- Module      : Network.AWS.WorkDocs.DeleteNotificationSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription from the specified organization.
module Network.AWS.WorkDocs.DeleteNotificationSubscription
  ( -- * Creating a Request
    DeleteNotificationSubscription (..),
    newDeleteNotificationSubscription,

    -- * Request Lenses
    deleteNotificationSubscription_subscriptionId,
    deleteNotificationSubscription_organizationId,

    -- * Destructuring the Response
    DeleteNotificationSubscriptionResponse (..),
    newDeleteNotificationSubscriptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteNotificationSubscription' smart constructor.
data DeleteNotificationSubscription = DeleteNotificationSubscription'
  { -- | The ID of the subscription.
    subscriptionId :: Prelude.Text,
    -- | The ID of the organization.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionId', 'deleteNotificationSubscription_subscriptionId' - The ID of the subscription.
--
-- 'organizationId', 'deleteNotificationSubscription_organizationId' - The ID of the organization.
newDeleteNotificationSubscription ::
  -- | 'subscriptionId'
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  DeleteNotificationSubscription
newDeleteNotificationSubscription
  pSubscriptionId_
  pOrganizationId_ =
    DeleteNotificationSubscription'
      { subscriptionId =
          pSubscriptionId_,
        organizationId = pOrganizationId_
      }

-- | The ID of the subscription.
deleteNotificationSubscription_subscriptionId :: Lens.Lens' DeleteNotificationSubscription Prelude.Text
deleteNotificationSubscription_subscriptionId = Lens.lens (\DeleteNotificationSubscription' {subscriptionId} -> subscriptionId) (\s@DeleteNotificationSubscription' {} a -> s {subscriptionId = a} :: DeleteNotificationSubscription)

-- | The ID of the organization.
deleteNotificationSubscription_organizationId :: Lens.Lens' DeleteNotificationSubscription Prelude.Text
deleteNotificationSubscription_organizationId = Lens.lens (\DeleteNotificationSubscription' {organizationId} -> organizationId) (\s@DeleteNotificationSubscription' {} a -> s {organizationId = a} :: DeleteNotificationSubscription)

instance
  Prelude.AWSRequest
    DeleteNotificationSubscription
  where
  type
    Rs DeleteNotificationSubscription =
      DeleteNotificationSubscriptionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteNotificationSubscriptionResponse'

instance
  Prelude.Hashable
    DeleteNotificationSubscription

instance
  Prelude.NFData
    DeleteNotificationSubscription

instance
  Prelude.ToHeaders
    DeleteNotificationSubscription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToPath
    DeleteNotificationSubscription
  where
  toPath DeleteNotificationSubscription' {..} =
    Prelude.mconcat
      [ "/api/v1/organizations/",
        Prelude.toBS organizationId,
        "/subscriptions/",
        Prelude.toBS subscriptionId
      ]

instance
  Prelude.ToQuery
    DeleteNotificationSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotificationSubscriptionResponse' smart constructor.
data DeleteNotificationSubscriptionResponse = DeleteNotificationSubscriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationSubscriptionResponse ::
  DeleteNotificationSubscriptionResponse
newDeleteNotificationSubscriptionResponse =
  DeleteNotificationSubscriptionResponse'

instance
  Prelude.NFData
    DeleteNotificationSubscriptionResponse
