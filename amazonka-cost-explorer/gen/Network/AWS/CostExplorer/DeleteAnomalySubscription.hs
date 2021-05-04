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
-- Module      : Network.AWS.CostExplorer.DeleteAnomalySubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly subscription.
module Network.AWS.CostExplorer.DeleteAnomalySubscription
  ( -- * Creating a Request
    DeleteAnomalySubscription (..),
    newDeleteAnomalySubscription,

    -- * Request Lenses
    deleteAnomalySubscription_subscriptionArn,

    -- * Destructuring the Response
    DeleteAnomalySubscriptionResponse (..),
    newDeleteAnomalySubscriptionResponse,

    -- * Response Lenses
    deleteAnomalySubscriptionResponse_httpStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAnomalySubscription' smart constructor.
data DeleteAnomalySubscription = DeleteAnomalySubscription'
  { -- | The unique identifier of the cost anomaly subscription that you want to
    -- delete.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionArn', 'deleteAnomalySubscription_subscriptionArn' - The unique identifier of the cost anomaly subscription that you want to
-- delete.
newDeleteAnomalySubscription ::
  -- | 'subscriptionArn'
  Prelude.Text ->
  DeleteAnomalySubscription
newDeleteAnomalySubscription pSubscriptionArn_ =
  DeleteAnomalySubscription'
    { subscriptionArn =
        pSubscriptionArn_
    }

-- | The unique identifier of the cost anomaly subscription that you want to
-- delete.
deleteAnomalySubscription_subscriptionArn :: Lens.Lens' DeleteAnomalySubscription Prelude.Text
deleteAnomalySubscription_subscriptionArn = Lens.lens (\DeleteAnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@DeleteAnomalySubscription' {} a -> s {subscriptionArn = a} :: DeleteAnomalySubscription)

instance Prelude.AWSRequest DeleteAnomalySubscription where
  type
    Rs DeleteAnomalySubscription =
      DeleteAnomalySubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAnomalySubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnomalySubscription

instance Prelude.NFData DeleteAnomalySubscription

instance Prelude.ToHeaders DeleteAnomalySubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.DeleteAnomalySubscription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAnomalySubscription where
  toJSON DeleteAnomalySubscription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SubscriptionArn" Prelude..= subscriptionArn)
          ]
      )

instance Prelude.ToPath DeleteAnomalySubscription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAnomalySubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAnomalySubscriptionResponse' smart constructor.
data DeleteAnomalySubscriptionResponse = DeleteAnomalySubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalySubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnomalySubscriptionResponse_httpStatus' - The response's http status code.
newDeleteAnomalySubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAnomalySubscriptionResponse
newDeleteAnomalySubscriptionResponse pHttpStatus_ =
  DeleteAnomalySubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAnomalySubscriptionResponse_httpStatus :: Lens.Lens' DeleteAnomalySubscriptionResponse Prelude.Int
deleteAnomalySubscriptionResponse_httpStatus = Lens.lens (\DeleteAnomalySubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteAnomalySubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteAnomalySubscriptionResponse)

instance
  Prelude.NFData
    DeleteAnomalySubscriptionResponse
