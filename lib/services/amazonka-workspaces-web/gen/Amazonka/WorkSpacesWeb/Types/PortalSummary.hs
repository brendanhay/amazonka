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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.PortalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.BrowserType
import Amazonka.WorkSpacesWeb.Types.PortalStatus
import Amazonka.WorkSpacesWeb.Types.RendererType

-- | The summary of the portal.
--
-- /See:/ 'newPortalSummary' smart constructor.
data PortalSummary = PortalSummary'
  { -- | The ARN of the trust that is associated with this web portal.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the web portal.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The creation date of the web portal.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The endpoint URL of the web portal that users access in order to start
    -- streaming sessions.
    portalEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user access logging settings that is associated with the
    -- web portal.
    userAccessLoggingSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the browser settings that is associated with the web portal.
    browserSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user settings that is associated with the web portal.
    userSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the web portal.
    portalStatus :: Prelude.Maybe PortalStatus,
    -- | The browser type of the web portal.
    browserType :: Prelude.Maybe BrowserType,
    -- | The ARN of the network settings that is associated with the web portal.
    networkSettingsArn :: Prelude.Maybe Prelude.Text,
    -- | The renderer that is used in streaming sessions.
    rendererType :: Prelude.Maybe RendererType
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
-- 'trustStoreArn', 'portalSummary_trustStoreArn' - The ARN of the trust that is associated with this web portal.
--
-- 'portalArn', 'portalSummary_portalArn' - The ARN of the web portal.
--
-- 'displayName', 'portalSummary_displayName' - The name of the web portal.
--
-- 'creationDate', 'portalSummary_creationDate' - The creation date of the web portal.
--
-- 'portalEndpoint', 'portalSummary_portalEndpoint' - The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
--
-- 'userAccessLoggingSettingsArn', 'portalSummary_userAccessLoggingSettingsArn' - The ARN of the user access logging settings that is associated with the
-- web portal.
--
-- 'browserSettingsArn', 'portalSummary_browserSettingsArn' - The ARN of the browser settings that is associated with the web portal.
--
-- 'userSettingsArn', 'portalSummary_userSettingsArn' - The ARN of the user settings that is associated with the web portal.
--
-- 'portalStatus', 'portalSummary_portalStatus' - The status of the web portal.
--
-- 'browserType', 'portalSummary_browserType' - The browser type of the web portal.
--
-- 'networkSettingsArn', 'portalSummary_networkSettingsArn' - The ARN of the network settings that is associated with the web portal.
--
-- 'rendererType', 'portalSummary_rendererType' - The renderer that is used in streaming sessions.
newPortalSummary ::
  PortalSummary
newPortalSummary =
  PortalSummary'
    { trustStoreArn = Prelude.Nothing,
      portalArn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      portalEndpoint = Prelude.Nothing,
      userAccessLoggingSettingsArn = Prelude.Nothing,
      browserSettingsArn = Prelude.Nothing,
      userSettingsArn = Prelude.Nothing,
      portalStatus = Prelude.Nothing,
      browserType = Prelude.Nothing,
      networkSettingsArn = Prelude.Nothing,
      rendererType = Prelude.Nothing
    }

-- | The ARN of the trust that is associated with this web portal.
portalSummary_trustStoreArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_trustStoreArn = Lens.lens (\PortalSummary' {trustStoreArn} -> trustStoreArn) (\s@PortalSummary' {} a -> s {trustStoreArn = a} :: PortalSummary)

-- | The ARN of the web portal.
portalSummary_portalArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_portalArn = Lens.lens (\PortalSummary' {portalArn} -> portalArn) (\s@PortalSummary' {} a -> s {portalArn = a} :: PortalSummary)

-- | The name of the web portal.
portalSummary_displayName :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_displayName = Lens.lens (\PortalSummary' {displayName} -> displayName) (\s@PortalSummary' {} a -> s {displayName = a} :: PortalSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The creation date of the web portal.
portalSummary_creationDate :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.UTCTime)
portalSummary_creationDate = Lens.lens (\PortalSummary' {creationDate} -> creationDate) (\s@PortalSummary' {} a -> s {creationDate = a} :: PortalSummary) Prelude.. Lens.mapping Core._Time

-- | The endpoint URL of the web portal that users access in order to start
-- streaming sessions.
portalSummary_portalEndpoint :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_portalEndpoint = Lens.lens (\PortalSummary' {portalEndpoint} -> portalEndpoint) (\s@PortalSummary' {} a -> s {portalEndpoint = a} :: PortalSummary)

-- | The ARN of the user access logging settings that is associated with the
-- web portal.
portalSummary_userAccessLoggingSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_userAccessLoggingSettingsArn = Lens.lens (\PortalSummary' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@PortalSummary' {} a -> s {userAccessLoggingSettingsArn = a} :: PortalSummary)

-- | The ARN of the browser settings that is associated with the web portal.
portalSummary_browserSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_browserSettingsArn = Lens.lens (\PortalSummary' {browserSettingsArn} -> browserSettingsArn) (\s@PortalSummary' {} a -> s {browserSettingsArn = a} :: PortalSummary)

-- | The ARN of the user settings that is associated with the web portal.
portalSummary_userSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_userSettingsArn = Lens.lens (\PortalSummary' {userSettingsArn} -> userSettingsArn) (\s@PortalSummary' {} a -> s {userSettingsArn = a} :: PortalSummary)

-- | The status of the web portal.
portalSummary_portalStatus :: Lens.Lens' PortalSummary (Prelude.Maybe PortalStatus)
portalSummary_portalStatus = Lens.lens (\PortalSummary' {portalStatus} -> portalStatus) (\s@PortalSummary' {} a -> s {portalStatus = a} :: PortalSummary)

-- | The browser type of the web portal.
portalSummary_browserType :: Lens.Lens' PortalSummary (Prelude.Maybe BrowserType)
portalSummary_browserType = Lens.lens (\PortalSummary' {browserType} -> browserType) (\s@PortalSummary' {} a -> s {browserType = a} :: PortalSummary)

-- | The ARN of the network settings that is associated with the web portal.
portalSummary_networkSettingsArn :: Lens.Lens' PortalSummary (Prelude.Maybe Prelude.Text)
portalSummary_networkSettingsArn = Lens.lens (\PortalSummary' {networkSettingsArn} -> networkSettingsArn) (\s@PortalSummary' {} a -> s {networkSettingsArn = a} :: PortalSummary)

-- | The renderer that is used in streaming sessions.
portalSummary_rendererType :: Lens.Lens' PortalSummary (Prelude.Maybe RendererType)
portalSummary_rendererType = Lens.lens (\PortalSummary' {rendererType} -> rendererType) (\s@PortalSummary' {} a -> s {rendererType = a} :: PortalSummary)

instance Core.FromJSON PortalSummary where
  parseJSON =
    Core.withObject
      "PortalSummary"
      ( \x ->
          PortalSummary'
            Prelude.<$> (x Core..:? "trustStoreArn")
            Prelude.<*> (x Core..:? "portalArn")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "portalEndpoint")
            Prelude.<*> (x Core..:? "userAccessLoggingSettingsArn")
            Prelude.<*> (x Core..:? "browserSettingsArn")
            Prelude.<*> (x Core..:? "userSettingsArn")
            Prelude.<*> (x Core..:? "portalStatus")
            Prelude.<*> (x Core..:? "browserType")
            Prelude.<*> (x Core..:? "networkSettingsArn")
            Prelude.<*> (x Core..:? "rendererType")
      )

instance Prelude.Hashable PortalSummary where
  hashWithSalt _salt PortalSummary' {..} =
    _salt `Prelude.hashWithSalt` trustStoreArn
      `Prelude.hashWithSalt` portalArn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` portalEndpoint
      `Prelude.hashWithSalt` userAccessLoggingSettingsArn
      `Prelude.hashWithSalt` browserSettingsArn
      `Prelude.hashWithSalt` userSettingsArn
      `Prelude.hashWithSalt` portalStatus
      `Prelude.hashWithSalt` browserType
      `Prelude.hashWithSalt` networkSettingsArn
      `Prelude.hashWithSalt` rendererType

instance Prelude.NFData PortalSummary where
  rnf PortalSummary' {..} =
    Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf portalEndpoint
      `Prelude.seq` Prelude.rnf userAccessLoggingSettingsArn
      `Prelude.seq` Prelude.rnf browserSettingsArn
      `Prelude.seq` Prelude.rnf userSettingsArn
      `Prelude.seq` Prelude.rnf portalStatus
      `Prelude.seq` Prelude.rnf browserType
      `Prelude.seq` Prelude.rnf networkSettingsArn
      `Prelude.seq` Prelude.rnf rendererType
