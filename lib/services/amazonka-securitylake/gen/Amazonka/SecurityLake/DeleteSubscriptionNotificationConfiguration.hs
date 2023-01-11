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
-- Module      : Amazonka.SecurityLake.DeleteSubscriptionNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.DeleteSubscriptionNotificationConfiguration
  ( -- * Creating a Request
    DeleteSubscriptionNotificationConfiguration (..),
    newDeleteSubscriptionNotificationConfiguration,

    -- * Request Lenses
    deleteSubscriptionNotificationConfiguration_subscriptionId,

    -- * Destructuring the Response
    DeleteSubscriptionNotificationConfigurationResponse (..),
    newDeleteSubscriptionNotificationConfigurationResponse,

    -- * Response Lenses
    deleteSubscriptionNotificationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteSubscriptionNotificationConfiguration' smart constructor.
data DeleteSubscriptionNotificationConfiguration = DeleteSubscriptionNotificationConfiguration'
  { -- | The ID of the Security Lake subscriber account.
    subscriptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriptionNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionId', 'deleteSubscriptionNotificationConfiguration_subscriptionId' - The ID of the Security Lake subscriber account.
newDeleteSubscriptionNotificationConfiguration ::
  -- | 'subscriptionId'
  Prelude.Text ->
  DeleteSubscriptionNotificationConfiguration
newDeleteSubscriptionNotificationConfiguration
  pSubscriptionId_ =
    DeleteSubscriptionNotificationConfiguration'
      { subscriptionId =
          pSubscriptionId_
      }

-- | The ID of the Security Lake subscriber account.
deleteSubscriptionNotificationConfiguration_subscriptionId :: Lens.Lens' DeleteSubscriptionNotificationConfiguration Prelude.Text
deleteSubscriptionNotificationConfiguration_subscriptionId = Lens.lens (\DeleteSubscriptionNotificationConfiguration' {subscriptionId} -> subscriptionId) (\s@DeleteSubscriptionNotificationConfiguration' {} a -> s {subscriptionId = a} :: DeleteSubscriptionNotificationConfiguration)

instance
  Core.AWSRequest
    DeleteSubscriptionNotificationConfiguration
  where
  type
    AWSResponse
      DeleteSubscriptionNotificationConfiguration =
      DeleteSubscriptionNotificationConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriptionNotificationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteSubscriptionNotificationConfiguration
  where
  hashWithSalt
    _salt
    DeleteSubscriptionNotificationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` subscriptionId

instance
  Prelude.NFData
    DeleteSubscriptionNotificationConfiguration
  where
  rnf DeleteSubscriptionNotificationConfiguration' {..} =
    Prelude.rnf subscriptionId

instance
  Data.ToHeaders
    DeleteSubscriptionNotificationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteSubscriptionNotificationConfiguration
  where
  toPath
    DeleteSubscriptionNotificationConfiguration' {..} =
      Prelude.mconcat
        [ "/subscription-notifications/",
          Data.toBS subscriptionId
        ]

instance
  Data.ToQuery
    DeleteSubscriptionNotificationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSubscriptionNotificationConfigurationResponse' smart constructor.
data DeleteSubscriptionNotificationConfigurationResponse = DeleteSubscriptionNotificationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriptionNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSubscriptionNotificationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteSubscriptionNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSubscriptionNotificationConfigurationResponse
newDeleteSubscriptionNotificationConfigurationResponse
  pHttpStatus_ =
    DeleteSubscriptionNotificationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteSubscriptionNotificationConfigurationResponse_httpStatus :: Lens.Lens' DeleteSubscriptionNotificationConfigurationResponse Prelude.Int
deleteSubscriptionNotificationConfigurationResponse_httpStatus = Lens.lens (\DeleteSubscriptionNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriptionNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteSubscriptionNotificationConfigurationResponse)

instance
  Prelude.NFData
    DeleteSubscriptionNotificationConfigurationResponse
  where
  rnf
    DeleteSubscriptionNotificationConfigurationResponse' {..} =
      Prelude.rnf httpStatus
