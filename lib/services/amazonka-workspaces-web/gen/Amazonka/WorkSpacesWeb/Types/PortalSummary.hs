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
-- Module      : Amazonka.WorkSpacesWeb.Types.PortalSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.PortalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.AuthenticationType
import Amazonka.WorkSpacesWeb.Types.BrowserType
import Amazonka.WorkSpacesWeb.Types.PortalStatus
import Amazonka.WorkSpacesWeb.Types.RendererType

-- | The summary of the portal.
--
-- /See:/ 'newPortalSummary' smart constructor.
data PortalSummary = PortalSummary'
  { -- | The type of authentication integration points used when signing into the
    -- web portal. Defaults to @Standard@.
    --
    -- @Standard@ web portals are authenticated directly through your identity
    -- provider. You need to call @CreateIdentityProvider@ to integrate your
    -- identity provider with your web portal. User and group access to your
    -- web portal is controlled through your identity provider.
    --
    -- @IAM_Identity_Center@ web portals are authenticated through AWS IAM
    -- Identity Center (successor to AWS Single Sign-On). They provide
    -- additional features, such as IdP-initiated authentication. Identity
    -- sources (including external identity provider integration), plus user
    -- and group access to your web portal, can be configured in the IAM
    -- Identity Center.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The ARN of the browser settings that is associated with the web portal.
    browserSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The browser type of the web portal.
    browserType :: Prelude.Maybe BrowserType,
    -- | The creation date of the web portal.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the web portal.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the IP access settings.
    ipAccessSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the network settings that is associated with the web portal.
    networkSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Maybe Prelude.Text,
    -- | The endpoint URL of the web portal that users access in order to start
    -- streaming sessions.
    portalEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The status of the web portal.
    portalStatus :: Prelude.Maybe PortalStatus,
    -- | The renderer that is used in streaming sessions.
    rendererType :: Prelude.Maybe RendererType,
    -- | The ARN of the trust that is associated with this web portal.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user access logging settings that is associated with the
    -- web portal.
    userAccessLoggingSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user settings that is associated with the web portal.
    userSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationType', 'portalSummary_authenticationType' - The type of authentication integration points used when signing into the
-- web portal. Defaults to @Standard@.
--
-- @Standard@ web portals are authenticated directly through your identity
-- provider. You need to call @CreateIdentityProvider@ to integrate your
-- identity provider with your web portal. User and group access to your
-- web portal is controlled through your identity provider.
--
-- @IAM_Identity_Center@ web portals are authenticated through AWS IAM
-- Identity Center (successor to AWS Single Sign-On). They provide
-- additional features, such as IdP-initiated authentication. Identity
-- sources (including external identity provider integration), plus user
-- and group access to your web portal, can be configured in the IAM
-- Identity Center.
--
-- 'browserSettingsArn', 'portalSummary_browserSettingsArn' - The ARN of the browser settings that is associated with the web portal.
--
-- 'browserType', 'portalSummary_browserType' - The browser type of the web portal.
--
-- 'creationDate', 'portalSummary_creationDate' - The creation date of the web portal.
--
-- 'displayName', 'portalSummary_displayName' - The name of the web portal.
--
-- 'ipAccessSettingsArn', 'portalSummary_ipAccessSettingsArn' - The ARN of the IP access settings.
--
-- 'networkSettingsArn', 'portalSummary_networkSettingsArn' - The ARN of the network settings that is associated with the web portal.
--
-- 'portalArn', 'portalSummary_portalArn' - The ARN of the web portal.
--
-- 'portalEndpoint', 'portalSummary_portalEndpoint' - The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
--
-- 'portalStatus', 'portalSummary_portalStatus' - The status of the web portal.
--
-- 'rendererType', 'portalSummary_rendererType' - The renderer that is used in streaming sessions.
--
-- 'trustStoreArn', 'portalSummary_trustStoreArn' - The ARN of the trust that is associated with this web portal.
--
-- 'userAccessLoggingSettingsArn', 'portalSummary_userAccessLoggingSettingsArn' - The ARN of the user access logging settings that is associated with the
-- web portal.
--
-- 'userSettingsArn', 'portalSummary_userSettingsArn' - The ARN of the user settings that is associated with the web portal.
newPortalSummary ::
  PortalSummary
newPortalSummary =
  PortalSummary'
    { authenticationType =
        Prelude.Nothing,
      browserSettingsArn = Prelude.Nothing,
      browserType = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      ipAccessSettingsArn = Prelude.Nothing,
      networkSettingsArn = Prelude.Nothing,
      portalArn = Prelude.Nothing,
      portalEndpoint = Prelude.Nothing,
      portalStatus = Prelude.Nothing,
      rendererType = Prelude.Nothing,
      trustStoreArn = Prelude.Nothing,
      userAccessLoggingSettingsArn = Prelude.Nothing,
      userSettingsArn = Prelude.Nothing
    }

-- | The type of authentication integration points used when signing into the
-- web portal. Defaults to @Standard@.
--
-- @Standard@ web portals are authenticated directly through your identity
-- provider. You need to call @CreateIdentityProvider@ to integrate your
-- identity provider with your web portal. User and group access to your
-- web portal is controlled through your identity provider.
--
-- @IAM_Identity_Center@ web portals are authenticated through AWS IAM
-- Identity Center (successor to AWS Single Sign-On). They provide
-- additional features, such as IdP-initiated authentication. Identity
-- sources (including external identity provider integration), plus user
-- and group access to your web portal, can be configured in the IAM
-- Identity Center.
portalSummary_authenticationType :: Lens.Lens' PortalSummary (Prelude.Maybe AuthenticationType)
portalSummary_authenticationType = Lens.lens (\PortalSummary' {authenticationType} -> authenticationType) (\s@PortalSummary' {} a -> s {authenticationType = a} :: PortalSummary)

-- | The ARN of the browser settings that is associated with the web portal.
portalSummary_browserSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_browserSettingsArn = Lens.lens (\PortalSummary' {browserSettingsArn} -> browserSettingsArn) (\s@PortalSummary' {} a -> s {browserSettingsArn = a} :: PortalSummary)

-- | The browser type of the web portal.
portalSummary_browserType :: Lens.Lens' PortalSummary (Prelude.Maybe BrowserType)
portalSummary_browserType = Lens.lens (\PortalSummary' {browserType} -> browserType) (\s@PortalSummary' {} a -> s {browserType = a} :: PortalSummary)

-- | The creation date of the web portal.
portalSummary_creationDate :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.UTCTime)
portalSummary_creationDate = Lens.lens (\PortalSummary' {creationDate} -> creationDate) (\s@PortalSummary' {} a -> s {creationDate = a} :: PortalSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the web portal.
portalSummary_displayName :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_displayName = Lens.lens (\PortalSummary' {displayName} -> displayName) (\s@PortalSummary' {} a -> s {displayName = a} :: PortalSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the IP access settings.
portalSummary_ipAccessSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_ipAccessSettingsArn = Lens.lens (\PortalSummary' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@PortalSummary' {} a -> s {ipAccessSettingsArn = a} :: PortalSummary)

-- | The ARN of the network settings that is associated with the web portal.
portalSummary_networkSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_networkSettingsArn = Lens.lens (\PortalSummary' {networkSettingsArn} -> networkSettingsArn) (\s@PortalSummary' {} a -> s {networkSettingsArn = a} :: PortalSummary)

-- | The ARN of the web portal.
portalSummary_portalArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_portalArn = Lens.lens (\PortalSummary' {portalArn} -> portalArn) (\s@PortalSummary' {} a -> s {portalArn = a} :: PortalSummary)

-- | The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
portalSummary_portalEndpoint :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_portalEndpoint = Lens.lens (\PortalSummary' {portalEndpoint} -> portalEndpoint) (\s@PortalSummary' {} a -> s {portalEndpoint = a} :: PortalSummary)

-- | The status of the web portal.
portalSummary_portalStatus :: Lens.Lens' PortalSummary (Prelude.Maybe PortalStatus)
portalSummary_portalStatus = Lens.lens (\PortalSummary' {portalStatus} -> portalStatus) (\s@PortalSummary' {} a -> s {portalStatus = a} :: PortalSummary)

-- | The renderer that is used in streaming sessions.
portalSummary_rendererType :: Lens.Lens' PortalSummary (Prelude.Maybe RendererType)
portalSummary_rendererType = Lens.lens (\PortalSummary' {rendererType} -> rendererType) (\s@PortalSummary' {} a -> s {rendererType = a} :: PortalSummary)

-- | The ARN of the trust that is associated with this web portal.
portalSummary_trustStoreArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_trustStoreArn = Lens.lens (\PortalSummary' {trustStoreArn} -> trustStoreArn) (\s@PortalSummary' {} a -> s {trustStoreArn = a} :: PortalSummary)

-- | The ARN of the user access logging settings that is associated with the
-- web portal.
portalSummary_userAccessLoggingSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_userAccessLoggingSettingsArn = Lens.lens (\PortalSummary' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@PortalSummary' {} a -> s {userAccessLoggingSettingsArn = a} :: PortalSummary)

-- | The ARN of the user settings that is associated with the web portal.
portalSummary_userSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_userSettingsArn = Lens.lens (\PortalSummary' {userSettingsArn} -> userSettingsArn) (\s@PortalSummary' {} a -> s {userSettingsArn = a} :: PortalSummary)

instance Data.FromJSON PortalSummary where
  parseJSON =
    Data.withObject
      "PortalSummary"
      ( \x ->
          PortalSummary'
            Prelude.<$> (x Data..:? "authenticationType")
            Prelude.<*> (x Data..:? "browserSettingsArn")
            Prelude.<*> (x Data..:? "browserType")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "ipAccessSettingsArn")
            Prelude.<*> (x Data..:? "networkSettingsArn")
            Prelude.<*> (x Data..:? "portalArn")
            Prelude.<*> (x Data..:? "portalEndpoint")
            Prelude.<*> (x Data..:? "portalStatus")
            Prelude.<*> (x Data..:? "rendererType")
            Prelude.<*> (x Data..:? "trustStoreArn")
            Prelude.<*> (x Data..:? "userAccessLoggingSettingsArn")
            Prelude.<*> (x Data..:? "userSettingsArn")
      )

instance Prelude.Hashable PortalSummary where
  hashWithSalt _salt PortalSummary' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` browserSettingsArn
      `Prelude.hashWithSalt` browserType
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` ipAccessSettingsArn
      `Prelude.hashWithSalt` networkSettingsArn
      `Prelude.hashWithSalt` portalArn
      `Prelude.hashWithSalt` portalEndpoint
      `Prelude.hashWithSalt` portalStatus
      `Prelude.hashWithSalt` rendererType
      `Prelude.hashWithSalt` trustStoreArn
      `Prelude.hashWithSalt` userAccessLoggingSettingsArn
      `Prelude.hashWithSalt` userSettingsArn

instance Prelude.NFData PortalSummary where
  rnf PortalSummary' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf browserSettingsArn
      `Prelude.seq` Prelude.rnf browserType
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf ipAccessSettingsArn
      `Prelude.seq` Prelude.rnf networkSettingsArn
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf portalEndpoint
      `Prelude.seq` Prelude.rnf portalStatus
      `Prelude.seq` Prelude.rnf rendererType
      `Prelude.seq` Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf userAccessLoggingSettingsArn
      `Prelude.seq` Prelude.rnf userSettingsArn
