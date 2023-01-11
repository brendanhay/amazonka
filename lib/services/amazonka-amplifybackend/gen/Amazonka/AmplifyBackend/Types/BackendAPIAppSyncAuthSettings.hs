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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The authentication settings for accessing provisioned data models in
-- your Amplify project.
--
-- /See:/ 'newBackendAPIAppSyncAuthSettings' smart constructor.
data BackendAPIAppSyncAuthSettings = BackendAPIAppSyncAuthSettings'
  { -- | The Amazon Cognito user pool ID, if Amazon Cognito was used as an
    -- authentication setting to access your data models.
    cognitoUserPoolId :: Prelude.Maybe Prelude.Text,
    -- | The API key description for API_KEY, if it was used as an authentication
    -- mechanism to access your data models.
    description :: Prelude.Maybe Prelude.Text,
    -- | The API key expiration time for API_KEY, if it was used as an
    -- authentication mechanism to access your data models.
    expirationTime :: Prelude.Maybe Prelude.Double,
    -- | The expiry time for the OpenID authentication mechanism.
    openIDAuthTTL :: Prelude.Maybe Prelude.Text,
    -- | The clientID for openID, if openID was used as an authentication setting
    -- to access your data models.
    openIDClientId :: Prelude.Maybe Prelude.Text,
    -- | The expiry time for the OpenID authentication mechanism.
    openIDIatTTL :: Prelude.Maybe Prelude.Text,
    -- | The openID issuer URL, if openID was used as an authentication setting
    -- to access your data models.
    openIDIssueURL :: Prelude.Maybe Prelude.Text,
    -- | The OpenID provider name, if OpenID was used as an authentication
    -- mechanism to access your data models.
    openIDProviderName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAPIAppSyncAuthSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoUserPoolId', 'backendAPIAppSyncAuthSettings_cognitoUserPoolId' - The Amazon Cognito user pool ID, if Amazon Cognito was used as an
-- authentication setting to access your data models.
--
-- 'description', 'backendAPIAppSyncAuthSettings_description' - The API key description for API_KEY, if it was used as an authentication
-- mechanism to access your data models.
--
-- 'expirationTime', 'backendAPIAppSyncAuthSettings_expirationTime' - The API key expiration time for API_KEY, if it was used as an
-- authentication mechanism to access your data models.
--
-- 'openIDAuthTTL', 'backendAPIAppSyncAuthSettings_openIDAuthTTL' - The expiry time for the OpenID authentication mechanism.
--
-- 'openIDClientId', 'backendAPIAppSyncAuthSettings_openIDClientId' - The clientID for openID, if openID was used as an authentication setting
-- to access your data models.
--
-- 'openIDIatTTL', 'backendAPIAppSyncAuthSettings_openIDIatTTL' - The expiry time for the OpenID authentication mechanism.
--
-- 'openIDIssueURL', 'backendAPIAppSyncAuthSettings_openIDIssueURL' - The openID issuer URL, if openID was used as an authentication setting
-- to access your data models.
--
-- 'openIDProviderName', 'backendAPIAppSyncAuthSettings_openIDProviderName' - The OpenID provider name, if OpenID was used as an authentication
-- mechanism to access your data models.
newBackendAPIAppSyncAuthSettings ::
  BackendAPIAppSyncAuthSettings
newBackendAPIAppSyncAuthSettings =
  BackendAPIAppSyncAuthSettings'
    { cognitoUserPoolId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      openIDAuthTTL = Prelude.Nothing,
      openIDClientId = Prelude.Nothing,
      openIDIatTTL = Prelude.Nothing,
      openIDIssueURL = Prelude.Nothing,
      openIDProviderName = Prelude.Nothing
    }

-- | The Amazon Cognito user pool ID, if Amazon Cognito was used as an
-- authentication setting to access your data models.
backendAPIAppSyncAuthSettings_cognitoUserPoolId :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_cognitoUserPoolId = Lens.lens (\BackendAPIAppSyncAuthSettings' {cognitoUserPoolId} -> cognitoUserPoolId) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {cognitoUserPoolId = a} :: BackendAPIAppSyncAuthSettings)

-- | The API key description for API_KEY, if it was used as an authentication
-- mechanism to access your data models.
backendAPIAppSyncAuthSettings_description :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_description = Lens.lens (\BackendAPIAppSyncAuthSettings' {description} -> description) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {description = a} :: BackendAPIAppSyncAuthSettings)

-- | The API key expiration time for API_KEY, if it was used as an
-- authentication mechanism to access your data models.
backendAPIAppSyncAuthSettings_expirationTime :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Double)
backendAPIAppSyncAuthSettings_expirationTime = Lens.lens (\BackendAPIAppSyncAuthSettings' {expirationTime} -> expirationTime) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {expirationTime = a} :: BackendAPIAppSyncAuthSettings)

-- | The expiry time for the OpenID authentication mechanism.
backendAPIAppSyncAuthSettings_openIDAuthTTL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDAuthTTL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDAuthTTL} -> openIDAuthTTL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDAuthTTL = a} :: BackendAPIAppSyncAuthSettings)

-- | The clientID for openID, if openID was used as an authentication setting
-- to access your data models.
backendAPIAppSyncAuthSettings_openIDClientId :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDClientId = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDClientId} -> openIDClientId) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDClientId = a} :: BackendAPIAppSyncAuthSettings)

-- | The expiry time for the OpenID authentication mechanism.
backendAPIAppSyncAuthSettings_openIDIatTTL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDIatTTL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDIatTTL} -> openIDIatTTL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDIatTTL = a} :: BackendAPIAppSyncAuthSettings)

-- | The openID issuer URL, if openID was used as an authentication setting
-- to access your data models.
backendAPIAppSyncAuthSettings_openIDIssueURL :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDIssueURL = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDIssueURL} -> openIDIssueURL) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDIssueURL = a} :: BackendAPIAppSyncAuthSettings)

-- | The OpenID provider name, if OpenID was used as an authentication
-- mechanism to access your data models.
backendAPIAppSyncAuthSettings_openIDProviderName :: Lens.Lens' BackendAPIAppSyncAuthSettings (Prelude.Maybe Prelude.Text)
backendAPIAppSyncAuthSettings_openIDProviderName = Lens.lens (\BackendAPIAppSyncAuthSettings' {openIDProviderName} -> openIDProviderName) (\s@BackendAPIAppSyncAuthSettings' {} a -> s {openIDProviderName = a} :: BackendAPIAppSyncAuthSettings)

instance Data.FromJSON BackendAPIAppSyncAuthSettings where
  parseJSON =
    Data.withObject
      "BackendAPIAppSyncAuthSettings"
      ( \x ->
          BackendAPIAppSyncAuthSettings'
            Prelude.<$> (x Data..:? "cognitoUserPoolId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "expirationTime")
            Prelude.<*> (x Data..:? "openIDAuthTTL")
            Prelude.<*> (x Data..:? "openIDClientId")
            Prelude.<*> (x Data..:? "openIDIatTTL")
            Prelude.<*> (x Data..:? "openIDIssueURL")
            Prelude.<*> (x Data..:? "openIDProviderName")
      )

instance
  Prelude.Hashable
    BackendAPIAppSyncAuthSettings
  where
  hashWithSalt _salt BackendAPIAppSyncAuthSettings' {..} =
    _salt `Prelude.hashWithSalt` cognitoUserPoolId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` openIDAuthTTL
      `Prelude.hashWithSalt` openIDClientId
      `Prelude.hashWithSalt` openIDIatTTL
      `Prelude.hashWithSalt` openIDIssueURL
      `Prelude.hashWithSalt` openIDProviderName

instance Prelude.NFData BackendAPIAppSyncAuthSettings where
  rnf BackendAPIAppSyncAuthSettings' {..} =
    Prelude.rnf cognitoUserPoolId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf openIDAuthTTL
      `Prelude.seq` Prelude.rnf openIDClientId
      `Prelude.seq` Prelude.rnf openIDIatTTL
      `Prelude.seq` Prelude.rnf openIDIssueURL
      `Prelude.seq` Prelude.rnf openIDProviderName

instance Data.ToJSON BackendAPIAppSyncAuthSettings where
  toJSON BackendAPIAppSyncAuthSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cognitoUserPoolId" Data..=)
              Prelude.<$> cognitoUserPoolId,
            ("description" Data..=) Prelude.<$> description,
            ("expirationTime" Data..=)
              Prelude.<$> expirationTime,
            ("openIDAuthTTL" Data..=) Prelude.<$> openIDAuthTTL,
            ("openIDClientId" Data..=)
              Prelude.<$> openIDClientId,
            ("openIDIatTTL" Data..=) Prelude.<$> openIDIatTTL,
            ("openIDIssueURL" Data..=)
              Prelude.<$> openIDIssueURL,
            ("openIDProviderName" Data..=)
              Prelude.<$> openIDProviderName
          ]
      )
