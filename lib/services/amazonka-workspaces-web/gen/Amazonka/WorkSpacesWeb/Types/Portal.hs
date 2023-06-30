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
-- Module      : Amazonka.WorkSpacesWeb.Types.Portal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.Portal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.BrowserType
import Amazonka.WorkSpacesWeb.Types.PortalStatus
import Amazonka.WorkSpacesWeb.Types.RendererType

-- | The web portal.
--
-- /See:/ 'newPortal' smart constructor.
data Portal = Portal'
  { -- | The ARN of the browser settings that is associated with this web portal.
    browserSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The browser that users see when using a streaming session.
    browserType :: Prelude.Maybe BrowserType,
    -- | The creation date of the web portal.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the web portal.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    -- | A message that explains why the web portal is in its current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the trust store that is associated with the web portal.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user access logging settings that is associated with the
    -- web portal.
    userAccessLoggingSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user settings that is associated with the web portal.
    userSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Portal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettingsArn', 'portal_browserSettingsArn' - The ARN of the browser settings that is associated with this web portal.
--
-- 'browserType', 'portal_browserType' - The browser that users see when using a streaming session.
--
-- 'creationDate', 'portal_creationDate' - The creation date of the web portal.
--
-- 'displayName', 'portal_displayName' - The name of the web portal.
--
-- 'networkSettingsArn', 'portal_networkSettingsArn' - The ARN of the network settings that is associated with the web portal.
--
-- 'portalArn', 'portal_portalArn' - The ARN of the web portal.
--
-- 'portalEndpoint', 'portal_portalEndpoint' - The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
--
-- 'portalStatus', 'portal_portalStatus' - The status of the web portal.
--
-- 'rendererType', 'portal_rendererType' - The renderer that is used in streaming sessions.
--
-- 'statusReason', 'portal_statusReason' - A message that explains why the web portal is in its current status.
--
-- 'trustStoreArn', 'portal_trustStoreArn' - The ARN of the trust store that is associated with the web portal.
--
-- 'userAccessLoggingSettingsArn', 'portal_userAccessLoggingSettingsArn' - The ARN of the user access logging settings that is associated with the
-- web portal.
--
-- 'userSettingsArn', 'portal_userSettingsArn' - The ARN of the user settings that is associated with the web portal.
newPortal ::
  Portal
newPortal =
  Portal'
    { browserSettingsArn = Prelude.Nothing,
      browserType = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      networkSettingsArn = Prelude.Nothing,
      portalArn = Prelude.Nothing,
      portalEndpoint = Prelude.Nothing,
      portalStatus = Prelude.Nothing,
      rendererType = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      trustStoreArn = Prelude.Nothing,
      userAccessLoggingSettingsArn = Prelude.Nothing,
      userSettingsArn = Prelude.Nothing
    }

-- | The ARN of the browser settings that is associated with this web portal.
portal_browserSettingsArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_browserSettingsArn = Lens.lens (\Portal' {browserSettingsArn} -> browserSettingsArn) (\s@Portal' {} a -> s {browserSettingsArn = a} :: Portal)

-- | The browser that users see when using a streaming session.
portal_browserType :: Lens.Lens' Portal (Prelude.Maybe BrowserType)
portal_browserType = Lens.lens (\Portal' {browserType} -> browserType) (\s@Portal' {} a -> s {browserType = a} :: Portal)

-- | The creation date of the web portal.
portal_creationDate :: Lens.Lens' Portal (Prelude.Maybe Prelude.UTCTime)
portal_creationDate = Lens.lens (\Portal' {creationDate} -> creationDate) (\s@Portal' {} a -> s {creationDate = a} :: Portal) Prelude.. Lens.mapping Data._Time

-- | The name of the web portal.
portal_displayName :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_displayName = Lens.lens (\Portal' {displayName} -> displayName) (\s@Portal' {} a -> s {displayName = a} :: Portal) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the network settings that is associated with the web portal.
portal_networkSettingsArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_networkSettingsArn = Lens.lens (\Portal' {networkSettingsArn} -> networkSettingsArn) (\s@Portal' {} a -> s {networkSettingsArn = a} :: Portal)

-- | The ARN of the web portal.
portal_portalArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_portalArn = Lens.lens (\Portal' {portalArn} -> portalArn) (\s@Portal' {} a -> s {portalArn = a} :: Portal)

-- | The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
portal_portalEndpoint :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_portalEndpoint = Lens.lens (\Portal' {portalEndpoint} -> portalEndpoint) (\s@Portal' {} a -> s {portalEndpoint = a} :: Portal)

-- | The status of the web portal.
portal_portalStatus :: Lens.Lens' Portal (Prelude.Maybe PortalStatus)
portal_portalStatus = Lens.lens (\Portal' {portalStatus} -> portalStatus) (\s@Portal' {} a -> s {portalStatus = a} :: Portal)

-- | The renderer that is used in streaming sessions.
portal_rendererType :: Lens.Lens' Portal (Prelude.Maybe RendererType)
portal_rendererType = Lens.lens (\Portal' {rendererType} -> rendererType) (\s@Portal' {} a -> s {rendererType = a} :: Portal)

-- | A message that explains why the web portal is in its current status.
portal_statusReason :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_statusReason = Lens.lens (\Portal' {statusReason} -> statusReason) (\s@Portal' {} a -> s {statusReason = a} :: Portal)

-- | The ARN of the trust store that is associated with the web portal.
portal_trustStoreArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_trustStoreArn = Lens.lens (\Portal' {trustStoreArn} -> trustStoreArn) (\s@Portal' {} a -> s {trustStoreArn = a} :: Portal)

-- | The ARN of the user access logging settings that is associated with the
-- web portal.
portal_userAccessLoggingSettingsArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_userAccessLoggingSettingsArn = Lens.lens (\Portal' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@Portal' {} a -> s {userAccessLoggingSettingsArn = a} :: Portal)

-- | The ARN of the user settings that is associated with the web portal.
portal_userSettingsArn :: Lens.Lens' Portal (Prelude.Maybe Prelude.Text)
portal_userSettingsArn = Lens.lens (\Portal' {userSettingsArn} -> userSettingsArn) (\s@Portal' {} a -> s {userSettingsArn = a} :: Portal)

instance Data.FromJSON Portal where
  parseJSON =
    Data.withObject
      "Portal"
      ( \x ->
          Portal'
            Prelude.<$> (x Data..:? "browserSettingsArn")
            Prelude.<*> (x Data..:? "browserType")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "networkSettingsArn")
            Prelude.<*> (x Data..:? "portalArn")
            Prelude.<*> (x Data..:? "portalEndpoint")
            Prelude.<*> (x Data..:? "portalStatus")
            Prelude.<*> (x Data..:? "rendererType")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "trustStoreArn")
            Prelude.<*> (x Data..:? "userAccessLoggingSettingsArn")
            Prelude.<*> (x Data..:? "userSettingsArn")
      )

instance Prelude.Hashable Portal where
  hashWithSalt _salt Portal' {..} =
    _salt
      `Prelude.hashWithSalt` browserSettingsArn
      `Prelude.hashWithSalt` browserType
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` networkSettingsArn
      `Prelude.hashWithSalt` portalArn
      `Prelude.hashWithSalt` portalEndpoint
      `Prelude.hashWithSalt` portalStatus
      `Prelude.hashWithSalt` rendererType
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` trustStoreArn
      `Prelude.hashWithSalt` userAccessLoggingSettingsArn
      `Prelude.hashWithSalt` userSettingsArn

instance Prelude.NFData Portal where
  rnf Portal' {..} =
    Prelude.rnf browserSettingsArn
      `Prelude.seq` Prelude.rnf browserType
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf networkSettingsArn
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf portalEndpoint
      `Prelude.seq` Prelude.rnf portalStatus
      `Prelude.seq` Prelude.rnf rendererType
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf userAccessLoggingSettingsArn
      `Prelude.seq` Prelude.rnf userSettingsArn
