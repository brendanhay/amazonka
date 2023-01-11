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
-- Module      : Amazonka.WorkSpacesWeb.Types.BrowserSettingsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.BrowserSettingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary for browser settings.
--
-- /See:/ 'newBrowserSettingsSummary' smart constructor.
data BrowserSettingsSummary = BrowserSettingsSummary'
  { -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrowserSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettingsArn', 'browserSettingsSummary_browserSettingsArn' - The ARN of the browser settings.
newBrowserSettingsSummary ::
  BrowserSettingsSummary
newBrowserSettingsSummary =
  BrowserSettingsSummary'
    { browserSettingsArn =
        Prelude.Nothing
    }

-- | The ARN of the browser settings.
browserSettingsSummary_browserSettingsArn :: Lens.Lens' BrowserSettingsSummary (Prelude.Maybe Prelude.Text)
browserSettingsSummary_browserSettingsArn = Lens.lens (\BrowserSettingsSummary' {browserSettingsArn} -> browserSettingsArn) (\s@BrowserSettingsSummary' {} a -> s {browserSettingsArn = a} :: BrowserSettingsSummary)

instance Data.FromJSON BrowserSettingsSummary where
  parseJSON =
    Data.withObject
      "BrowserSettingsSummary"
      ( \x ->
          BrowserSettingsSummary'
            Prelude.<$> (x Data..:? "browserSettingsArn")
      )

instance Prelude.Hashable BrowserSettingsSummary where
  hashWithSalt _salt BrowserSettingsSummary' {..} =
    _salt `Prelude.hashWithSalt` browserSettingsArn

instance Prelude.NFData BrowserSettingsSummary where
  rnf BrowserSettingsSummary' {..} =
    Prelude.rnf browserSettingsArn
