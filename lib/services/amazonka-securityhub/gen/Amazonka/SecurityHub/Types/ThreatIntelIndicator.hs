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
-- Module      : Amazonka.SecurityHub.Types.ThreatIntelIndicator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ThreatIntelIndicator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ThreatIntelIndicatorCategory
import Amazonka.SecurityHub.Types.ThreatIntelIndicatorType

-- | Details about the threat intelligence related to a finding.
--
-- /See:/ 'newThreatIntelIndicator' smart constructor.
data ThreatIntelIndicator = ThreatIntelIndicator'
  { -- | The category of a threat intelligence indicator.
    category :: Prelude.Maybe ThreatIntelIndicatorCategory,
    -- | Indicates when the most recent instance of a threat intelligence
    -- indicator was observed.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    lastObservedAt :: Prelude.Maybe Prelude.Text,
    -- | The source of the threat intelligence indicator.
    source :: Prelude.Maybe Prelude.Text,
    -- | The URL to the page or site where you can get more information about the
    -- threat intelligence indicator.
    sourceUrl :: Prelude.Maybe Prelude.Text,
    -- | The type of threat intelligence indicator.
    type' :: Prelude.Maybe ThreatIntelIndicatorType,
    -- | The value of a threat intelligence indicator.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThreatIntelIndicator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'threatIntelIndicator_category' - The category of a threat intelligence indicator.
--
-- 'lastObservedAt', 'threatIntelIndicator_lastObservedAt' - Indicates when the most recent instance of a threat intelligence
-- indicator was observed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'source', 'threatIntelIndicator_source' - The source of the threat intelligence indicator.
--
-- 'sourceUrl', 'threatIntelIndicator_sourceUrl' - The URL to the page or site where you can get more information about the
-- threat intelligence indicator.
--
-- 'type'', 'threatIntelIndicator_type' - The type of threat intelligence indicator.
--
-- 'value', 'threatIntelIndicator_value' - The value of a threat intelligence indicator.
newThreatIntelIndicator ::
  ThreatIntelIndicator
newThreatIntelIndicator =
  ThreatIntelIndicator'
    { category = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceUrl = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The category of a threat intelligence indicator.
threatIntelIndicator_category :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe ThreatIntelIndicatorCategory)
threatIntelIndicator_category = Lens.lens (\ThreatIntelIndicator' {category} -> category) (\s@ThreatIntelIndicator' {} a -> s {category = a} :: ThreatIntelIndicator)

-- | Indicates when the most recent instance of a threat intelligence
-- indicator was observed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
threatIntelIndicator_lastObservedAt :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe Prelude.Text)
threatIntelIndicator_lastObservedAt = Lens.lens (\ThreatIntelIndicator' {lastObservedAt} -> lastObservedAt) (\s@ThreatIntelIndicator' {} a -> s {lastObservedAt = a} :: ThreatIntelIndicator)

-- | The source of the threat intelligence indicator.
threatIntelIndicator_source :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe Prelude.Text)
threatIntelIndicator_source = Lens.lens (\ThreatIntelIndicator' {source} -> source) (\s@ThreatIntelIndicator' {} a -> s {source = a} :: ThreatIntelIndicator)

-- | The URL to the page or site where you can get more information about the
-- threat intelligence indicator.
threatIntelIndicator_sourceUrl :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe Prelude.Text)
threatIntelIndicator_sourceUrl = Lens.lens (\ThreatIntelIndicator' {sourceUrl} -> sourceUrl) (\s@ThreatIntelIndicator' {} a -> s {sourceUrl = a} :: ThreatIntelIndicator)

-- | The type of threat intelligence indicator.
threatIntelIndicator_type :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe ThreatIntelIndicatorType)
threatIntelIndicator_type = Lens.lens (\ThreatIntelIndicator' {type'} -> type') (\s@ThreatIntelIndicator' {} a -> s {type' = a} :: ThreatIntelIndicator)

-- | The value of a threat intelligence indicator.
threatIntelIndicator_value :: Lens.Lens' ThreatIntelIndicator (Prelude.Maybe Prelude.Text)
threatIntelIndicator_value = Lens.lens (\ThreatIntelIndicator' {value} -> value) (\s@ThreatIntelIndicator' {} a -> s {value = a} :: ThreatIntelIndicator)

instance Data.FromJSON ThreatIntelIndicator where
  parseJSON =
    Data.withObject
      "ThreatIntelIndicator"
      ( \x ->
          ThreatIntelIndicator'
            Prelude.<$> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "LastObservedAt")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "SourceUrl")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ThreatIntelIndicator where
  hashWithSalt _salt ThreatIntelIndicator' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceUrl
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData ThreatIntelIndicator where
  rnf ThreatIntelIndicator' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf lastObservedAt
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf sourceUrl
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ThreatIntelIndicator where
  toJSON ThreatIntelIndicator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Category" Data..=) Prelude.<$> category,
            ("LastObservedAt" Data..=)
              Prelude.<$> lastObservedAt,
            ("Source" Data..=) Prelude.<$> source,
            ("SourceUrl" Data..=) Prelude.<$> sourceUrl,
            ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )
