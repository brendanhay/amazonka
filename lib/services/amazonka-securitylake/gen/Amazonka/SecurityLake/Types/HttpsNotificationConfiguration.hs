{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityLake.Types.HttpsNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.HttpsNotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.HttpMethod

-- | The configurations for HTTPS subscriber notification.
--
-- /See:/ 'newHttpsNotificationConfiguration' smart constructor.
data HttpsNotificationConfiguration = HttpsNotificationConfiguration'
  { -- | The key name for the notification subscription.
    authorizationApiKeyName :: Prelude.Maybe Prelude.Text,
    -- | The key value for the notification subscription.
    authorizationApiKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The HTTPS method used for the notification subscription.
    httpMethod :: Prelude.Maybe HttpMethod,
    -- | The subscription endpoint in Security Lake. If you prefer notification
    -- with an HTTPs endpoint, populate this field.
    endpoint :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
    -- role that you created. For more information about ARNs and how to use
    -- them in policies, see
    -- <https://docs.aws.amazon.com//security-lake/latest/userguide/subscriber-data-access.html Managing data access>
    -- and
    -- <https://docs.aws.amazon.com/security-lake/latest/userguide/security-iam-awsmanpol.html Amazon Web Services Managed Policies>
    -- in the Amazon Security Lake User Guide.
    targetRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpsNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationApiKeyName', 'httpsNotificationConfiguration_authorizationApiKeyName' - The key name for the notification subscription.
--
-- 'authorizationApiKeyValue', 'httpsNotificationConfiguration_authorizationApiKeyValue' - The key value for the notification subscription.
--
-- 'httpMethod', 'httpsNotificationConfiguration_httpMethod' - The HTTPS method used for the notification subscription.
--
-- 'endpoint', 'httpsNotificationConfiguration_endpoint' - The subscription endpoint in Security Lake. If you prefer notification
-- with an HTTPs endpoint, populate this field.
--
-- 'targetRoleArn', 'httpsNotificationConfiguration_targetRoleArn' - The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
-- role that you created. For more information about ARNs and how to use
-- them in policies, see
-- <https://docs.aws.amazon.com//security-lake/latest/userguide/subscriber-data-access.html Managing data access>
-- and
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/security-iam-awsmanpol.html Amazon Web Services Managed Policies>
-- in the Amazon Security Lake User Guide.
newHttpsNotificationConfiguration ::
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'targetRoleArn'
  Prelude.Text ->
  HttpsNotificationConfiguration
newHttpsNotificationConfiguration
  pEndpoint_
  pTargetRoleArn_ =
    HttpsNotificationConfiguration'
      { authorizationApiKeyName =
          Prelude.Nothing,
        authorizationApiKeyValue = Prelude.Nothing,
        httpMethod = Prelude.Nothing,
        endpoint = pEndpoint_,
        targetRoleArn = pTargetRoleArn_
      }

-- | The key name for the notification subscription.
httpsNotificationConfiguration_authorizationApiKeyName :: Lens.Lens' HttpsNotificationConfiguration (Prelude.Maybe Prelude.Text)
httpsNotificationConfiguration_authorizationApiKeyName = Lens.lens (\HttpsNotificationConfiguration' {authorizationApiKeyName} -> authorizationApiKeyName) (\s@HttpsNotificationConfiguration' {} a -> s {authorizationApiKeyName = a} :: HttpsNotificationConfiguration)

-- | The key value for the notification subscription.
httpsNotificationConfiguration_authorizationApiKeyValue :: Lens.Lens' HttpsNotificationConfiguration (Prelude.Maybe Prelude.Text)
httpsNotificationConfiguration_authorizationApiKeyValue = Lens.lens (\HttpsNotificationConfiguration' {authorizationApiKeyValue} -> authorizationApiKeyValue) (\s@HttpsNotificationConfiguration' {} a -> s {authorizationApiKeyValue = a} :: HttpsNotificationConfiguration)

-- | The HTTPS method used for the notification subscription.
httpsNotificationConfiguration_httpMethod :: Lens.Lens' HttpsNotificationConfiguration (Prelude.Maybe HttpMethod)
httpsNotificationConfiguration_httpMethod = Lens.lens (\HttpsNotificationConfiguration' {httpMethod} -> httpMethod) (\s@HttpsNotificationConfiguration' {} a -> s {httpMethod = a} :: HttpsNotificationConfiguration)

-- | The subscription endpoint in Security Lake. If you prefer notification
-- with an HTTPs endpoint, populate this field.
httpsNotificationConfiguration_endpoint :: Lens.Lens' HttpsNotificationConfiguration Prelude.Text
httpsNotificationConfiguration_endpoint = Lens.lens (\HttpsNotificationConfiguration' {endpoint} -> endpoint) (\s@HttpsNotificationConfiguration' {} a -> s {endpoint = a} :: HttpsNotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the EventBridge API destinations IAM
-- role that you created. For more information about ARNs and how to use
-- them in policies, see
-- <https://docs.aws.amazon.com//security-lake/latest/userguide/subscriber-data-access.html Managing data access>
-- and
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/security-iam-awsmanpol.html Amazon Web Services Managed Policies>
-- in the Amazon Security Lake User Guide.
httpsNotificationConfiguration_targetRoleArn :: Lens.Lens' HttpsNotificationConfiguration Prelude.Text
httpsNotificationConfiguration_targetRoleArn = Lens.lens (\HttpsNotificationConfiguration' {targetRoleArn} -> targetRoleArn) (\s@HttpsNotificationConfiguration' {} a -> s {targetRoleArn = a} :: HttpsNotificationConfiguration)

instance
  Prelude.Hashable
    HttpsNotificationConfiguration
  where
  hashWithSalt
    _salt
    HttpsNotificationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` authorizationApiKeyName
        `Prelude.hashWithSalt` authorizationApiKeyValue
        `Prelude.hashWithSalt` httpMethod
        `Prelude.hashWithSalt` endpoint
        `Prelude.hashWithSalt` targetRoleArn

instance
  Prelude.NFData
    HttpsNotificationConfiguration
  where
  rnf HttpsNotificationConfiguration' {..} =
    Prelude.rnf authorizationApiKeyName
      `Prelude.seq` Prelude.rnf authorizationApiKeyValue
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf targetRoleArn

instance Data.ToJSON HttpsNotificationConfiguration where
  toJSON HttpsNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizationApiKeyName" Data..=)
              Prelude.<$> authorizationApiKeyName,
            ("authorizationApiKeyValue" Data..=)
              Prelude.<$> authorizationApiKeyValue,
            ("httpMethod" Data..=) Prelude.<$> httpMethod,
            Prelude.Just ("endpoint" Data..= endpoint),
            Prelude.Just
              ("targetRoleArn" Data..= targetRoleArn)
          ]
      )
