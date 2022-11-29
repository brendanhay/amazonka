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
-- Module      : Amazonka.WorkSpacesWeb.Types.BrowserSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.BrowserSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The browser settings resource that can be associated with a web portal.
-- Once associated with a web portal, browser settings control how the
-- browser will behave once a user starts a streaming session for the web
-- portal.
--
-- /See:/ 'newBrowserSettings' smart constructor.
data BrowserSettings = BrowserSettings'
  { -- | A list of web portal ARNs that this browser settings is associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text],
    -- | A JSON string containing Chrome Enterprise policies that will be applied
    -- to all streaming sessions.
    browserPolicy :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedPortalArns', 'browserSettings_associatedPortalArns' - A list of web portal ARNs that this browser settings is associated with.
--
-- 'browserPolicy', 'browserSettings_browserPolicy' - A JSON string containing Chrome Enterprise policies that will be applied
-- to all streaming sessions.
--
-- 'browserSettingsArn', 'browserSettings_browserSettingsArn' - The ARN of the browser settings.
newBrowserSettings ::
  -- | 'browserSettingsArn'
  Prelude.Text ->
  BrowserSettings
newBrowserSettings pBrowserSettingsArn_ =
  BrowserSettings'
    { associatedPortalArns =
        Prelude.Nothing,
      browserPolicy = Prelude.Nothing,
      browserSettingsArn = pBrowserSettingsArn_
    }

-- | A list of web portal ARNs that this browser settings is associated with.
browserSettings_associatedPortalArns :: Lens.Lens' BrowserSettings (Prelude.Maybe [Prelude.Text])
browserSettings_associatedPortalArns = Lens.lens (\BrowserSettings' {associatedPortalArns} -> associatedPortalArns) (\s@BrowserSettings' {} a -> s {associatedPortalArns = a} :: BrowserSettings) Prelude.. Lens.mapping Lens.coerced

-- | A JSON string containing Chrome Enterprise policies that will be applied
-- to all streaming sessions.
browserSettings_browserPolicy :: Lens.Lens' BrowserSettings (Prelude.Maybe Prelude.Text)
browserSettings_browserPolicy = Lens.lens (\BrowserSettings' {browserPolicy} -> browserPolicy) (\s@BrowserSettings' {} a -> s {browserPolicy = a} :: BrowserSettings) Prelude.. Lens.mapping Core._Sensitive

-- | The ARN of the browser settings.
browserSettings_browserSettingsArn :: Lens.Lens' BrowserSettings Prelude.Text
browserSettings_browserSettingsArn = Lens.lens (\BrowserSettings' {browserSettingsArn} -> browserSettingsArn) (\s@BrowserSettings' {} a -> s {browserSettingsArn = a} :: BrowserSettings)

instance Core.FromJSON BrowserSettings where
  parseJSON =
    Core.withObject
      "BrowserSettings"
      ( \x ->
          BrowserSettings'
            Prelude.<$> ( x Core..:? "associatedPortalArns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "browserPolicy")
            Prelude.<*> (x Core..: "browserSettingsArn")
      )

instance Prelude.Hashable BrowserSettings where
  hashWithSalt _salt BrowserSettings' {..} =
    _salt `Prelude.hashWithSalt` associatedPortalArns
      `Prelude.hashWithSalt` browserPolicy
      `Prelude.hashWithSalt` browserSettingsArn

instance Prelude.NFData BrowserSettings where
  rnf BrowserSettings' {..} =
    Prelude.rnf associatedPortalArns
      `Prelude.seq` Prelude.rnf browserPolicy
      `Prelude.seq` Prelude.rnf browserSettingsArn
