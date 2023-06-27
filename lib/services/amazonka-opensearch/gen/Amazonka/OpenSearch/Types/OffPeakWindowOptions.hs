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
-- Module      : Amazonka.OpenSearch.Types.OffPeakWindowOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OffPeakWindowOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OffPeakWindow
import qualified Amazonka.Prelude as Prelude

-- | Options for a domain\'s
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_OffPeakWindow.html off-peak window>,
-- during which OpenSearch Service can perform mandatory configuration
-- changes on the domain.
--
-- /See:/ 'newOffPeakWindowOptions' smart constructor.
data OffPeakWindowOptions = OffPeakWindowOptions'
  { -- | Whether to enable an off-peak window.
    --
    -- This option is only available when modifying a domain created prior to
    -- February 16, 2023, not when creating a new domain. All domains created
    -- after this date have the off-peak window enabled by default. You can\'t
    -- disable the off-peak window after it\'s enabled for a domain.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Off-peak window settings for the domain.
    offPeakWindow :: Prelude.Maybe OffPeakWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OffPeakWindowOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'offPeakWindowOptions_enabled' - Whether to enable an off-peak window.
--
-- This option is only available when modifying a domain created prior to
-- February 16, 2023, not when creating a new domain. All domains created
-- after this date have the off-peak window enabled by default. You can\'t
-- disable the off-peak window after it\'s enabled for a domain.
--
-- 'offPeakWindow', 'offPeakWindowOptions_offPeakWindow' - Off-peak window settings for the domain.
newOffPeakWindowOptions ::
  OffPeakWindowOptions
newOffPeakWindowOptions =
  OffPeakWindowOptions'
    { enabled = Prelude.Nothing,
      offPeakWindow = Prelude.Nothing
    }

-- | Whether to enable an off-peak window.
--
-- This option is only available when modifying a domain created prior to
-- February 16, 2023, not when creating a new domain. All domains created
-- after this date have the off-peak window enabled by default. You can\'t
-- disable the off-peak window after it\'s enabled for a domain.
offPeakWindowOptions_enabled :: Lens.Lens' OffPeakWindowOptions (Prelude.Maybe Prelude.Bool)
offPeakWindowOptions_enabled = Lens.lens (\OffPeakWindowOptions' {enabled} -> enabled) (\s@OffPeakWindowOptions' {} a -> s {enabled = a} :: OffPeakWindowOptions)

-- | Off-peak window settings for the domain.
offPeakWindowOptions_offPeakWindow :: Lens.Lens' OffPeakWindowOptions (Prelude.Maybe OffPeakWindow)
offPeakWindowOptions_offPeakWindow = Lens.lens (\OffPeakWindowOptions' {offPeakWindow} -> offPeakWindow) (\s@OffPeakWindowOptions' {} a -> s {offPeakWindow = a} :: OffPeakWindowOptions)

instance Data.FromJSON OffPeakWindowOptions where
  parseJSON =
    Data.withObject
      "OffPeakWindowOptions"
      ( \x ->
          OffPeakWindowOptions'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "OffPeakWindow")
      )

instance Prelude.Hashable OffPeakWindowOptions where
  hashWithSalt _salt OffPeakWindowOptions' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` offPeakWindow

instance Prelude.NFData OffPeakWindowOptions where
  rnf OffPeakWindowOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf offPeakWindow

instance Data.ToJSON OffPeakWindowOptions where
  toJSON OffPeakWindowOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("OffPeakWindow" Data..=) Prelude.<$> offPeakWindow
          ]
      )
