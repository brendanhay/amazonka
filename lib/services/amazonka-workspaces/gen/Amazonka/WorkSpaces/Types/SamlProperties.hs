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
-- Module      : Amazonka.WorkSpaces.Types.SamlProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.SamlProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.SamlStatusEnum

-- | Describes the enablement status, user access URL, and relay state
-- parameter name that are used for configuring federation with an SAML 2.0
-- identity provider.
--
-- /See:/ 'newSamlProperties' smart constructor.
data SamlProperties = SamlProperties'
  { -- | The relay state parameter name supported by the SAML 2.0 identity
    -- provider (IdP). When the end user is redirected to the user access URL
    -- from the WorkSpaces client application, this relay state parameter name
    -- is appended as a query parameter to the URL along with the relay state
    -- endpoint to return the user to the client application session.
    --
    -- To use SAML 2.0 authentication with WorkSpaces, the IdP must support
    -- IdP-initiated deep linking for the relay state URL. Consult your IdP
    -- documentation for more information.
    relayStateParameterName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of SAML 2.0 authentication. These statuses include
    -- the following.
    --
    -- -   If the setting is @DISABLED@, end users will be directed to login
    --     with their directory credentials.
    --
    -- -   If the setting is @ENABLED@, end users will be directed to login via
    --     the user access URL. Users attempting to connect to WorkSpaces from
    --     a client application that does not support SAML 2.0 authentication
    --     will not be able to connect.
    --
    -- -   If the setting is @ENABLED_WITH_DIRECTORY_LOGIN_FALLBACK@, end users
    --     will be directed to login via the user access URL on supported
    --     client applications, but will not prevent clients that do not
    --     support SAML 2.0 authentication from connecting as if SAML 2.0
    --     authentication was disabled.
    status :: Prelude.Maybe SamlStatusEnum,
    -- | The SAML 2.0 identity provider (IdP) user access URL is the URL a user
    -- would navigate to in their web browser in order to federate from the IdP
    -- and directly access the application, without any SAML 2.0 service
    -- provider (SP) bindings.
    userAccessUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamlProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relayStateParameterName', 'samlProperties_relayStateParameterName' - The relay state parameter name supported by the SAML 2.0 identity
-- provider (IdP). When the end user is redirected to the user access URL
-- from the WorkSpaces client application, this relay state parameter name
-- is appended as a query parameter to the URL along with the relay state
-- endpoint to return the user to the client application session.
--
-- To use SAML 2.0 authentication with WorkSpaces, the IdP must support
-- IdP-initiated deep linking for the relay state URL. Consult your IdP
-- documentation for more information.
--
-- 'status', 'samlProperties_status' - Indicates the status of SAML 2.0 authentication. These statuses include
-- the following.
--
-- -   If the setting is @DISABLED@, end users will be directed to login
--     with their directory credentials.
--
-- -   If the setting is @ENABLED@, end users will be directed to login via
--     the user access URL. Users attempting to connect to WorkSpaces from
--     a client application that does not support SAML 2.0 authentication
--     will not be able to connect.
--
-- -   If the setting is @ENABLED_WITH_DIRECTORY_LOGIN_FALLBACK@, end users
--     will be directed to login via the user access URL on supported
--     client applications, but will not prevent clients that do not
--     support SAML 2.0 authentication from connecting as if SAML 2.0
--     authentication was disabled.
--
-- 'userAccessUrl', 'samlProperties_userAccessUrl' - The SAML 2.0 identity provider (IdP) user access URL is the URL a user
-- would navigate to in their web browser in order to federate from the IdP
-- and directly access the application, without any SAML 2.0 service
-- provider (SP) bindings.
newSamlProperties ::
  SamlProperties
newSamlProperties =
  SamlProperties'
    { relayStateParameterName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      userAccessUrl = Prelude.Nothing
    }

-- | The relay state parameter name supported by the SAML 2.0 identity
-- provider (IdP). When the end user is redirected to the user access URL
-- from the WorkSpaces client application, this relay state parameter name
-- is appended as a query parameter to the URL along with the relay state
-- endpoint to return the user to the client application session.
--
-- To use SAML 2.0 authentication with WorkSpaces, the IdP must support
-- IdP-initiated deep linking for the relay state URL. Consult your IdP
-- documentation for more information.
samlProperties_relayStateParameterName :: Lens.Lens' SamlProperties (Prelude.Maybe Prelude.Text)
samlProperties_relayStateParameterName = Lens.lens (\SamlProperties' {relayStateParameterName} -> relayStateParameterName) (\s@SamlProperties' {} a -> s {relayStateParameterName = a} :: SamlProperties)

-- | Indicates the status of SAML 2.0 authentication. These statuses include
-- the following.
--
-- -   If the setting is @DISABLED@, end users will be directed to login
--     with their directory credentials.
--
-- -   If the setting is @ENABLED@, end users will be directed to login via
--     the user access URL. Users attempting to connect to WorkSpaces from
--     a client application that does not support SAML 2.0 authentication
--     will not be able to connect.
--
-- -   If the setting is @ENABLED_WITH_DIRECTORY_LOGIN_FALLBACK@, end users
--     will be directed to login via the user access URL on supported
--     client applications, but will not prevent clients that do not
--     support SAML 2.0 authentication from connecting as if SAML 2.0
--     authentication was disabled.
samlProperties_status :: Lens.Lens' SamlProperties (Prelude.Maybe SamlStatusEnum)
samlProperties_status = Lens.lens (\SamlProperties' {status} -> status) (\s@SamlProperties' {} a -> s {status = a} :: SamlProperties)

-- | The SAML 2.0 identity provider (IdP) user access URL is the URL a user
-- would navigate to in their web browser in order to federate from the IdP
-- and directly access the application, without any SAML 2.0 service
-- provider (SP) bindings.
samlProperties_userAccessUrl :: Lens.Lens' SamlProperties (Prelude.Maybe Prelude.Text)
samlProperties_userAccessUrl = Lens.lens (\SamlProperties' {userAccessUrl} -> userAccessUrl) (\s@SamlProperties' {} a -> s {userAccessUrl = a} :: SamlProperties)

instance Data.FromJSON SamlProperties where
  parseJSON =
    Data.withObject
      "SamlProperties"
      ( \x ->
          SamlProperties'
            Prelude.<$> (x Data..:? "RelayStateParameterName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserAccessUrl")
      )

instance Prelude.Hashable SamlProperties where
  hashWithSalt _salt SamlProperties' {..} =
    _salt
      `Prelude.hashWithSalt` relayStateParameterName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userAccessUrl

instance Prelude.NFData SamlProperties where
  rnf SamlProperties' {..} =
    Prelude.rnf relayStateParameterName `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf userAccessUrl

instance Data.ToJSON SamlProperties where
  toJSON SamlProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RelayStateParameterName" Data..=)
              Prelude.<$> relayStateParameterName,
            ("Status" Data..=) Prelude.<$> status,
            ("UserAccessUrl" Data..=) Prelude.<$> userAccessUrl
          ]
      )
