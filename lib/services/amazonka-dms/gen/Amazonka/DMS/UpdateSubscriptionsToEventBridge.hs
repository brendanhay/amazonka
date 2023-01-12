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
-- Module      : Amazonka.DMS.UpdateSubscriptionsToEventBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Migrates 10 active and enabled Amazon SNS subscriptions at a time and
-- converts them to corresponding Amazon EventBridge rules. By default,
-- this operation migrates subscriptions only when all your replication
-- instance versions are 3.4.6 or higher. If any replication instances are
-- from versions earlier than 3.4.6, the operation raises an error and
-- tells you to upgrade these instances to version 3.4.6 or higher. To
-- enable migration regardless of version, set the @Force@ option to true.
-- However, if you don\'t upgrade instances earlier than version 3.4.6,
-- some types of events might not be available when you use Amazon
-- EventBridge.
--
-- To call this operation, make sure that you have certain permissions
-- added to your user account. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html#CHAP_Events-migrate-to-eventbridge Migrating event subscriptions to Amazon EventBridge>
-- in the /Amazon Web Services Database Migration Service User Guide/.
module Amazonka.DMS.UpdateSubscriptionsToEventBridge
  ( -- * Creating a Request
    UpdateSubscriptionsToEventBridge (..),
    newUpdateSubscriptionsToEventBridge,

    -- * Request Lenses
    updateSubscriptionsToEventBridge_forceMove,

    -- * Destructuring the Response
    UpdateSubscriptionsToEventBridgeResponse (..),
    newUpdateSubscriptionsToEventBridgeResponse,

    -- * Response Lenses
    updateSubscriptionsToEventBridgeResponse_result,
    updateSubscriptionsToEventBridgeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newUpdateSubscriptionsToEventBridge' smart constructor.
data UpdateSubscriptionsToEventBridge = UpdateSubscriptionsToEventBridge'
  { -- | When set to true, this operation migrates DMS subscriptions for Amazon
    -- SNS notifications no matter what your replication instance version is.
    -- If not set or set to false, this operation runs only when all your
    -- replication instances are from DMS version 3.4.6 or higher.
    forceMove :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriptionsToEventBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceMove', 'updateSubscriptionsToEventBridge_forceMove' - When set to true, this operation migrates DMS subscriptions for Amazon
-- SNS notifications no matter what your replication instance version is.
-- If not set or set to false, this operation runs only when all your
-- replication instances are from DMS version 3.4.6 or higher.
newUpdateSubscriptionsToEventBridge ::
  UpdateSubscriptionsToEventBridge
newUpdateSubscriptionsToEventBridge =
  UpdateSubscriptionsToEventBridge'
    { forceMove =
        Prelude.Nothing
    }

-- | When set to true, this operation migrates DMS subscriptions for Amazon
-- SNS notifications no matter what your replication instance version is.
-- If not set or set to false, this operation runs only when all your
-- replication instances are from DMS version 3.4.6 or higher.
updateSubscriptionsToEventBridge_forceMove :: Lens.Lens' UpdateSubscriptionsToEventBridge (Prelude.Maybe Prelude.Bool)
updateSubscriptionsToEventBridge_forceMove = Lens.lens (\UpdateSubscriptionsToEventBridge' {forceMove} -> forceMove) (\s@UpdateSubscriptionsToEventBridge' {} a -> s {forceMove = a} :: UpdateSubscriptionsToEventBridge)

instance
  Core.AWSRequest
    UpdateSubscriptionsToEventBridge
  where
  type
    AWSResponse UpdateSubscriptionsToEventBridge =
      UpdateSubscriptionsToEventBridgeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubscriptionsToEventBridgeResponse'
            Prelude.<$> (x Data..?> "Result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubscriptionsToEventBridge
  where
  hashWithSalt
    _salt
    UpdateSubscriptionsToEventBridge' {..} =
      _salt `Prelude.hashWithSalt` forceMove

instance
  Prelude.NFData
    UpdateSubscriptionsToEventBridge
  where
  rnf UpdateSubscriptionsToEventBridge' {..} =
    Prelude.rnf forceMove

instance
  Data.ToHeaders
    UpdateSubscriptionsToEventBridge
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.UpdateSubscriptionsToEventBridge" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSubscriptionsToEventBridge where
  toJSON UpdateSubscriptionsToEventBridge' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ForceMove" Data..=) Prelude.<$> forceMove]
      )

instance Data.ToPath UpdateSubscriptionsToEventBridge where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateSubscriptionsToEventBridge
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newUpdateSubscriptionsToEventBridgeResponse' smart constructor.
data UpdateSubscriptionsToEventBridgeResponse = UpdateSubscriptionsToEventBridgeResponse'
  { -- | A string that indicates how many event subscriptions were migrated and
    -- how many remain to be migrated.
    result :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriptionsToEventBridgeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'updateSubscriptionsToEventBridgeResponse_result' - A string that indicates how many event subscriptions were migrated and
-- how many remain to be migrated.
--
-- 'httpStatus', 'updateSubscriptionsToEventBridgeResponse_httpStatus' - The response's http status code.
newUpdateSubscriptionsToEventBridgeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubscriptionsToEventBridgeResponse
newUpdateSubscriptionsToEventBridgeResponse
  pHttpStatus_ =
    UpdateSubscriptionsToEventBridgeResponse'
      { result =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A string that indicates how many event subscriptions were migrated and
-- how many remain to be migrated.
updateSubscriptionsToEventBridgeResponse_result :: Lens.Lens' UpdateSubscriptionsToEventBridgeResponse (Prelude.Maybe Prelude.Text)
updateSubscriptionsToEventBridgeResponse_result = Lens.lens (\UpdateSubscriptionsToEventBridgeResponse' {result} -> result) (\s@UpdateSubscriptionsToEventBridgeResponse' {} a -> s {result = a} :: UpdateSubscriptionsToEventBridgeResponse)

-- | The response's http status code.
updateSubscriptionsToEventBridgeResponse_httpStatus :: Lens.Lens' UpdateSubscriptionsToEventBridgeResponse Prelude.Int
updateSubscriptionsToEventBridgeResponse_httpStatus = Lens.lens (\UpdateSubscriptionsToEventBridgeResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionsToEventBridgeResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionsToEventBridgeResponse)

instance
  Prelude.NFData
    UpdateSubscriptionsToEventBridgeResponse
  where
  rnf UpdateSubscriptionsToEventBridgeResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus
