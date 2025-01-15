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
-- Module      : Amazonka.SecurityLake.CreateSubscriptionNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the subscriber when new data is written to the data lake for
-- the sources that the subscriber consumes in Security Lake.
module Amazonka.SecurityLake.CreateSubscriptionNotificationConfiguration
  ( -- * Creating a Request
    CreateSubscriptionNotificationConfiguration (..),
    newCreateSubscriptionNotificationConfiguration,

    -- * Request Lenses
    createSubscriptionNotificationConfiguration_createSqs,
    createSubscriptionNotificationConfiguration_httpsApiKeyName,
    createSubscriptionNotificationConfiguration_httpsApiKeyValue,
    createSubscriptionNotificationConfiguration_httpsMethod,
    createSubscriptionNotificationConfiguration_roleArn,
    createSubscriptionNotificationConfiguration_subscriptionEndpoint,
    createSubscriptionNotificationConfiguration_subscriptionId,

    -- * Destructuring the Response
    CreateSubscriptionNotificationConfigurationResponse (..),
    newCreateSubscriptionNotificationConfigurationResponse,

    -- * Response Lenses
    createSubscriptionNotificationConfigurationResponse_queueArn,
    createSubscriptionNotificationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateSubscriptionNotificationConfiguration' smart constructor.
data CreateSubscriptionNotificationConfiguration = CreateSubscriptionNotificationConfiguration'
  { -- | Create an Amazon Simple Queue Service queue.
    createSqs :: Prelude.Maybe Prelude.Bool,
    -- | The key name for the notification subscription.
    httpsApiKeyName :: Prelude.Maybe Prelude.Text,
    -- | The key value for the notification subscription.
    httpsApiKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The HTTPS method used for the notification subscription.
    httpsMethod :: Prelude.Maybe HttpsMethod,
    -- | The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
    -- role that you created.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The subscription endpoint in Security Lake. If you prefer notification
    -- with an HTTPs endpoint, populate this field.
    subscriptionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscription ID for the notification subscription\/
    subscriptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createSqs', 'createSubscriptionNotificationConfiguration_createSqs' - Create an Amazon Simple Queue Service queue.
--
-- 'httpsApiKeyName', 'createSubscriptionNotificationConfiguration_httpsApiKeyName' - The key name for the notification subscription.
--
-- 'httpsApiKeyValue', 'createSubscriptionNotificationConfiguration_httpsApiKeyValue' - The key value for the notification subscription.
--
-- 'httpsMethod', 'createSubscriptionNotificationConfiguration_httpsMethod' - The HTTPS method used for the notification subscription.
--
-- 'roleArn', 'createSubscriptionNotificationConfiguration_roleArn' - The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
-- role that you created.
--
-- 'subscriptionEndpoint', 'createSubscriptionNotificationConfiguration_subscriptionEndpoint' - The subscription endpoint in Security Lake. If you prefer notification
-- with an HTTPs endpoint, populate this field.
--
-- 'subscriptionId', 'createSubscriptionNotificationConfiguration_subscriptionId' - The subscription ID for the notification subscription\/
newCreateSubscriptionNotificationConfiguration ::
  -- | 'subscriptionId'
  Prelude.Text ->
  CreateSubscriptionNotificationConfiguration
newCreateSubscriptionNotificationConfiguration
  pSubscriptionId_ =
    CreateSubscriptionNotificationConfiguration'
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

-- | Create an Amazon Simple Queue Service queue.
createSubscriptionNotificationConfiguration_createSqs :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Bool)
createSubscriptionNotificationConfiguration_createSqs = Lens.lens (\CreateSubscriptionNotificationConfiguration' {createSqs} -> createSqs) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {createSqs = a} :: CreateSubscriptionNotificationConfiguration)

-- | The key name for the notification subscription.
createSubscriptionNotificationConfiguration_httpsApiKeyName :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
createSubscriptionNotificationConfiguration_httpsApiKeyName = Lens.lens (\CreateSubscriptionNotificationConfiguration' {httpsApiKeyName} -> httpsApiKeyName) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {httpsApiKeyName = a} :: CreateSubscriptionNotificationConfiguration)

-- | The key value for the notification subscription.
createSubscriptionNotificationConfiguration_httpsApiKeyValue :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
createSubscriptionNotificationConfiguration_httpsApiKeyValue = Lens.lens (\CreateSubscriptionNotificationConfiguration' {httpsApiKeyValue} -> httpsApiKeyValue) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {httpsApiKeyValue = a} :: CreateSubscriptionNotificationConfiguration)

-- | The HTTPS method used for the notification subscription.
createSubscriptionNotificationConfiguration_httpsMethod :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe HttpsMethod)
createSubscriptionNotificationConfiguration_httpsMethod = Lens.lens (\CreateSubscriptionNotificationConfiguration' {httpsMethod} -> httpsMethod) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {httpsMethod = a} :: CreateSubscriptionNotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
-- role that you created.
createSubscriptionNotificationConfiguration_roleArn :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
createSubscriptionNotificationConfiguration_roleArn = Lens.lens (\CreateSubscriptionNotificationConfiguration' {roleArn} -> roleArn) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {roleArn = a} :: CreateSubscriptionNotificationConfiguration)

-- | The subscription endpoint in Security Lake. If you prefer notification
-- with an HTTPs endpoint, populate this field.
createSubscriptionNotificationConfiguration_subscriptionEndpoint :: Lens.Lens' CreateSubscriptionNotificationConfiguration (Prelude.Maybe Prelude.Text)
createSubscriptionNotificationConfiguration_subscriptionEndpoint = Lens.lens (\CreateSubscriptionNotificationConfiguration' {subscriptionEndpoint} -> subscriptionEndpoint) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {subscriptionEndpoint = a} :: CreateSubscriptionNotificationConfiguration)

-- | The subscription ID for the notification subscription\/
createSubscriptionNotificationConfiguration_subscriptionId :: Lens.Lens' CreateSubscriptionNotificationConfiguration Prelude.Text
createSubscriptionNotificationConfiguration_subscriptionId = Lens.lens (\CreateSubscriptionNotificationConfiguration' {subscriptionId} -> subscriptionId) (\s@CreateSubscriptionNotificationConfiguration' {} a -> s {subscriptionId = a} :: CreateSubscriptionNotificationConfiguration)

instance
  Core.AWSRequest
    CreateSubscriptionNotificationConfiguration
  where
  type
    AWSResponse
      CreateSubscriptionNotificationConfiguration =
      CreateSubscriptionNotificationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionNotificationConfigurationResponse'
            Prelude.<$> (x Data..?> "queueArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSubscriptionNotificationConfiguration
  where
  hashWithSalt
    _salt
    CreateSubscriptionNotificationConfiguration' {..} =
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
    CreateSubscriptionNotificationConfiguration
  where
  rnf CreateSubscriptionNotificationConfiguration' {..} =
    Prelude.rnf createSqs `Prelude.seq`
      Prelude.rnf httpsApiKeyName `Prelude.seq`
        Prelude.rnf httpsApiKeyValue `Prelude.seq`
          Prelude.rnf httpsMethod `Prelude.seq`
            Prelude.rnf roleArn `Prelude.seq`
              Prelude.rnf subscriptionEndpoint `Prelude.seq`
                Prelude.rnf subscriptionId

instance
  Data.ToHeaders
    CreateSubscriptionNotificationConfiguration
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
    CreateSubscriptionNotificationConfiguration
  where
  toJSON
    CreateSubscriptionNotificationConfiguration' {..} =
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
    CreateSubscriptionNotificationConfiguration
  where
  toPath
    CreateSubscriptionNotificationConfiguration' {..} =
      Prelude.mconcat
        [ "/subscription-notifications/",
          Data.toBS subscriptionId
        ]

instance
  Data.ToQuery
    CreateSubscriptionNotificationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionNotificationConfigurationResponse' smart constructor.
data CreateSubscriptionNotificationConfigurationResponse = CreateSubscriptionNotificationConfigurationResponse'
  { -- | Returns the Amazon Resource Name (ARN) of the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueArn', 'createSubscriptionNotificationConfigurationResponse_queueArn' - Returns the Amazon Resource Name (ARN) of the queue.
--
-- 'httpStatus', 'createSubscriptionNotificationConfigurationResponse_httpStatus' - The response's http status code.
newCreateSubscriptionNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriptionNotificationConfigurationResponse
newCreateSubscriptionNotificationConfigurationResponse
  pHttpStatus_ =
    CreateSubscriptionNotificationConfigurationResponse'
      { queueArn =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns the Amazon Resource Name (ARN) of the queue.
createSubscriptionNotificationConfigurationResponse_queueArn :: Lens.Lens' CreateSubscriptionNotificationConfigurationResponse (Prelude.Maybe Prelude.Text)
createSubscriptionNotificationConfigurationResponse_queueArn = Lens.lens (\CreateSubscriptionNotificationConfigurationResponse' {queueArn} -> queueArn) (\s@CreateSubscriptionNotificationConfigurationResponse' {} a -> s {queueArn = a} :: CreateSubscriptionNotificationConfigurationResponse)

-- | The response's http status code.
createSubscriptionNotificationConfigurationResponse_httpStatus :: Lens.Lens' CreateSubscriptionNotificationConfigurationResponse Prelude.Int
createSubscriptionNotificationConfigurationResponse_httpStatus = Lens.lens (\CreateSubscriptionNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionNotificationConfigurationResponse)

instance
  Prelude.NFData
    CreateSubscriptionNotificationConfigurationResponse
  where
  rnf
    CreateSubscriptionNotificationConfigurationResponse' {..} =
      Prelude.rnf queueArn `Prelude.seq`
        Prelude.rnf httpStatus
