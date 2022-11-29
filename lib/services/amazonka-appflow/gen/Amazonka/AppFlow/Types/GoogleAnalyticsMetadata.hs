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
-- Module      : Amazonka.AppFlow.Types.GoogleAnalyticsMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.GoogleAnalyticsMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Google Analytics.
--
-- /See:/ 'newGoogleAnalyticsMetadata' smart constructor.
data GoogleAnalyticsMetadata = GoogleAnalyticsMetadata'
  { -- | The desired authorization scope for the Google Analytics account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GoogleAnalyticsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthScopes', 'googleAnalyticsMetadata_oAuthScopes' - The desired authorization scope for the Google Analytics account.
newGoogleAnalyticsMetadata ::
  GoogleAnalyticsMetadata
newGoogleAnalyticsMetadata =
  GoogleAnalyticsMetadata'
    { oAuthScopes =
        Prelude.Nothing
    }

-- | The desired authorization scope for the Google Analytics account.
googleAnalyticsMetadata_oAuthScopes :: Lens.Lens' GoogleAnalyticsMetadata (Prelude.Maybe [Prelude.Text])
googleAnalyticsMetadata_oAuthScopes = Lens.lens (\GoogleAnalyticsMetadata' {oAuthScopes} -> oAuthScopes) (\s@GoogleAnalyticsMetadata' {} a -> s {oAuthScopes = a} :: GoogleAnalyticsMetadata) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON GoogleAnalyticsMetadata where
  parseJSON =
    Core.withObject
      "GoogleAnalyticsMetadata"
      ( \x ->
          GoogleAnalyticsMetadata'
            Prelude.<$> (x Core..:? "oAuthScopes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable GoogleAnalyticsMetadata where
  hashWithSalt _salt GoogleAnalyticsMetadata' {..} =
    _salt `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData GoogleAnalyticsMetadata where
  rnf GoogleAnalyticsMetadata' {..} =
    Prelude.rnf oAuthScopes
