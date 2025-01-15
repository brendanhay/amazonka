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
-- Module      : Amazonka.SecurityLake.UpdateSubscriptionNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subscription notification or adds the existing
-- subscription notification setting for the specified subscription ID.
module Amazonka.SecurityLake.UpdateSubscriptionNotificationConfiguration
  ( -- * Creating a Request
    UpdateSubscriptionNotificationConfiguration (..),
    newUpdateSubscriptionNotificationConfiguration,

    -- * Request Lenses
    updateSubscriptionNotificationConfiguration_createSqs,
    updateSubscriptionNotificationConfiguration_httpsApiKeyName,
    updateSubscriptionNotificationConfiguration_httpsApiKeyValue,
    updateSubscriptionNotificationConfiguration_httpsMethod,
    updateSubscriptionNotificationConfiguration_roleArn,
    updateSubscriptionNotificationConfiguration_subscriptionEndpoint,
    updateSubscriptionNotificationConfiguration_subscriptionId,

    -- * Destructuring the Response
    UpdateSubscriptionNotificationConfigurationResponse (..),
    newUpdateSubscriptionNotificationConfigurationResponse,

    -- * Response Lenses
    updateSubscriptionNotificationConfigurationResponse_queueArn,
    updateSubscriptionNotificationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateSubscriptionNotificationConfiguration' smart constructor.
data UpdateSubscriptionNotificationConfiguration = UpdateSubscriptionNotificationConfiguration'
  { -- | Create a new subscription notification for the specified subscription ID
    -- in Amazon Security Lake.
    createSqs :: Prelude.Maybe Prelude.Bool,
    -- | The key name for the subscription notification.
    httpsApiKeyName :: Prelude.Maybe Prelude.Text,
    -- | The key value for the subscription notification.
    httpsApiKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The HTTPS method used for the subscription notification.
    httpsMethod :: Prelude.Maybe HttpsMethod,
    -- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The subscription endpoint in Security Lake.
    subscriptionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscription ID for which the subscription notification is
    -- specified.
    subscriptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriptionNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createSqs', 'updateSubscriptionNotificationConfiguration_createSqs' - Create a new subscription notification for the specified subscription ID
-- in Amazon Security Lake.
--
-- 'httpsApiKeyName', 'updateSubscriptionNotificationConfiguration_httpsApiKeyName' - The key name for the subscription notification.
--
-- 'httpsApiKeyValue', 'updateSubscriptionNotificationConfiguration_httpsApiKeyValue' - The key value for the subscription notification.
--
-- 'httpsMethod', 'updateSubscriptionNotificationConfiguration_httpsMethod' - The HTTPS method used for the subscription notification.
--
-- 'roleArn', 'updateSubscriptionNotificationConfiguration_roleArn' - The Amazon Resource Name (ARN) specifying the role of the subscriber.
--
-- 'subscriptionEndpoint', 'updateSubscriptionNotificationConfiguration_subscriptionEndpoint' - The subscription endpoint in Security Lake.
--
-- 'subscriptionId', 'updateSubscriptionNotificationConfiguration_subscriptionId' - The subscription ID for which the subscription notification is
-- specified.
newUpdateSubscriptionNotificationConfiguration ::
  -- | 'subscriptionId'
  Prelude.Text ->
  UpdateSubscriptionNotificationConfiguration
newUpdateSubscriptionNotificationConfiguration
  pSubscriptionId_ =
    UpdateSubscriptionNotificationConfiguration'
      { createSqs =
          Prelude.Nothing,
        httpsApiKeyName =
          Prelude.Nothing,
        httpsApiKeyValue =
          Prelude.Nothing,
        httpsMethod = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        subscriptionEndpoint =
          Prelude.Nothing,
        subscriptionId =
          pSubscriptionId_
      }

-- | Create a new subscription notification for the specified subscription ID
-- in Amazon Security Lake.
updateSubscriptionNotificationConfiguration_createSqs :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Bool)
updateSubscriptionNotificationConfiguration_createSqs = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {createSqs} -> createSqs) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {createSqs = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The key name for the subscription notification.
updateSubscriptionNotificationConfiguration_httpsApiKeyName :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateSubscriptionNotificationConfiguration_httpsApiKeyName = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {httpsApiKeyName} -> httpsApiKeyName) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {httpsApiKeyName = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The key value for the subscription notification.
updateSubscriptionNotificationConfiguration_httpsApiKeyValue :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateSubscriptionNotificationConfiguration_httpsApiKeyValue = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {httpsApiKeyValue} -> httpsApiKeyValue) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {httpsApiKeyValue = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The HTTPS method used for the subscription notification.
updateSubscriptionNotificationConfiguration_httpsMethod :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe HttpsMethod)
updateSubscriptionNotificationConfiguration_httpsMethod = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {httpsMethod} -> httpsMethod) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {httpsMethod = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
updateSubscriptionNotificationConfiguration_roleArn :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateSubscriptionNotificationConfiguration_roleArn = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {roleArn} -> roleArn) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {roleArn = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The subscription endpoint in Security Lake.
updateSubscriptionNotificationConfiguration_subscriptionEndpoint :: Lens.Lens' UpdateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
updateSubscriptionNotificationConfiguration_subscriptionEndpoint = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {subscriptionEndpoint} -> subscriptionEndpoint) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {subscriptionEndpoint = a} :: UpdateSubscriptionNotificationConfiguration)

-- | The subscription ID for which the subscription notification is
-- specified.
updateSubscriptionNotificationConfiguration_subscriptionId :: Lens.Lens' UpdateSubscriptionNotificationConfiguration Prelude.Text
updateSubscriptionNotificationConfiguration_subscriptionId = Lens.lens (\UpdateSubscriptionNotificationConfiguration' {subscriptionId} -> subscriptionId) (\s@UpdateSubscriptionNotificationConfiguration' {} a -> s {subscriptionId = a} :: UpdateSubscriptionNotificationConfiguration)

instance
  Core.AWSRequest
    UpdateSubscriptionNotificationConfiguration
  where
  type
    AWSResponse
      UpdateSubscriptionNotificationConfiguration =
      UpdateSubscriptionNotificationConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubscriptionNotificationConfigurationResponse'
            Prelude.<$> (x Data..?> "queueArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubscriptionNotificationConfiguration
  where
  hashWithSalt
    _salt
    UpdateSubscriptionNotificationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` createSqs
        `Prelude.hashWithSalt` httpsApiKeyName
        `Prelude.hashWithSalt` httpsApiKeyValue
        `Prelude.hashWithSalt` httpsMethod
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` subscriptionEndpoint
        `Prelude.hashWithSalt` subscriptionId

instance
  Prelude.NFData
    UpdateSubscriptionNotificationConfiguration
  where
  rnf UpdateSubscriptionNotificationConfiguration' {..} =
    Prelude.rnf createSqs `Prelude.seq`
      Prelude.rnf httpsApiKeyName `Prelude.seq`
        Prelude.rnf httpsApiKeyValue `Prelude.seq`
          Prelude.rnf httpsMethod `Prelude.seq`
            Prelude.rnf roleArn `Prelude.seq`
              Prelude.rnf subscriptionEndpoint `Prelude.seq`
                Prelude.rnf subscriptionId

instance
  Data.ToHeaders
    UpdateSubscriptionNotificationConfiguration
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
  Data.ToJSON
    UpdateSubscriptionNotificationConfiguration
  where
  toJSON
    UpdateSubscriptionNotificationConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("createSqs" Data..=) Prelude.<$> createSqs,
              ("httpsApiKeyName" Data..=)
                Prelude.<$> httpsApiKeyName,
              ("httpsApiKeyValue" Data..=)
                Prelude.<$> httpsApiKeyValue,
              ("httpsMethod" Data..=) Prelude.<$> httpsMethod,
              ("roleArn" Data..=) Prelude.<$> roleArn,
              ("subscriptionEndpoint" Data..=)
                Prelude.<$> subscriptionEndpoint
            ]
        )

instance
  Data.ToPath
    UpdateSubscriptionNotificationConfiguration
  where
  toPath
    UpdateSubscriptionNotificationConfiguration' {..} =
      Prelude.mconcat
        [ "/subscription-notifications/",
          Data.toBS subscriptionId
        ]

instance
  Data.ToQuery
    UpdateSubscriptionNotificationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriptionNotificationConfigurationResponse' smart constructor.
data UpdateSubscriptionNotificationConfigurationResponse = UpdateSubscriptionNotificationConfigurationResponse'
  { -- | Returns the ARN of the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSubscriptionNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueArn', 'updateSubscriptionNotificationConfigurationResponse_queueArn' - Returns the ARN of the queue.
--
-- 'httpStatus', 'updateSubscriptionNotificationConfigurationResponse_httpStatus' - The response's http status code.
newUpdateSubscriptionNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSubscriptionNotificationConfigurationResponse
newUpdateSubscriptionNotificationConfigurationResponse
  pHttpStatus_ =
    UpdateSubscriptionNotificationConfigurationResponse'
      { queueArn =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns the ARN of the queue.
updateSubscriptionNotificationConfigurationResponse_queueArn :: Lens.Lens' UpdateSubscriptionNotificationConfigurationResponse (Prelude.Maybe Prelude.Text)
updateSubscriptionNotificationConfigurationResponse_queueArn = Lens.lens (\UpdateSubscriptionNotificationConfigurationResponse' {queueArn} -> queueArn) (\s@UpdateSubscriptionNotificationConfigurationResponse' {} a -> s {queueArn = a} :: UpdateSubscriptionNotificationConfigurationResponse)

-- | The response's http status code.
updateSubscriptionNotificationConfigurationResponse_httpStatus :: Lens.Lens' UpdateSubscriptionNotificationConfigurationResponse Prelude.Int
updateSubscriptionNotificationConfigurationResponse_httpStatus = Lens.lens (\UpdateSubscriptionNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionNotificationConfigurationResponse)

instance
  Prelude.NFData
    UpdateSubscriptionNotificationConfigurationResponse
  where
  rnf
    UpdateSubscriptionNotificationConfigurationResponse' {..} =
      Prelude.rnf queueArn `Prelude.seq`
        Prelude.rnf httpStatus
