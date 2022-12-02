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
-- Module      : Amazonka.SecurityHub.Types.Severity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Severity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.SeverityLabel

-- | The severity of the finding.
--
-- The finding provider can provide the initial severity. The finding
-- provider can only update the severity if it has not been updated using
-- @BatchUpdateFindings@.
--
-- The finding must have either @Label@ or @Normalized@ populated. If only
-- one of these attributes is populated, then Security Hub automatically
-- populates the other one. If neither attribute is populated, then the
-- finding is invalid. @Label@ is the preferred attribute.
--
-- /See:/ 'newSeverity' smart constructor.
data Severity = Severity'
  { -- | Deprecated. This attribute is being deprecated. Instead of providing
    -- @Product@, provide @Original@.
    --
    -- The native severity as defined by the Amazon Web Services service or
    -- integrated partner product that generated the finding.
    product :: Prelude.Maybe Prelude.Double,
    -- | The severity value of the finding. The allowed values are the following.
    --
    -- -   @INFORMATIONAL@ - No issue was found.
    --
    -- -   @LOW@ - The issue does not require action on its own.
    --
    -- -   @MEDIUM@ - The issue must be addressed but not urgently.
    --
    -- -   @HIGH@ - The issue must be addressed as a priority.
    --
    -- -   @CRITICAL@ - The issue must be remediated immediately to avoid it
    --     escalating.
    --
    -- If you provide @Normalized@ and do not provide @Label@, then @Label@ is
    -- set automatically as follows.
    --
    -- -   0 - @INFORMATIONAL@
    --
    -- -   1–39 - @LOW@
    --
    -- -   40–69 - @MEDIUM@
    --
    -- -   70–89 - @HIGH@
    --
    -- -   90–100 - @CRITICAL@
    label :: Prelude.Maybe SeverityLabel,
    -- | Deprecated. The normalized severity of a finding. This attribute is
    -- being deprecated. Instead of providing @Normalized@, provide @Label@.
    --
    -- If you provide @Label@ and do not provide @Normalized@, then
    -- @Normalized@ is set automatically as follows.
    --
    -- -   @INFORMATIONAL@ - 0
    --
    -- -   @LOW@ - 1
    --
    -- -   @MEDIUM@ - 40
    --
    -- -   @HIGH@ - 70
    --
    -- -   @CRITICAL@ - 90
    normalized :: Prelude.Maybe Prelude.Int,
    -- | The native severity from the finding product that generated the finding.
    original :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Severity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'product', 'severity_product' - Deprecated. This attribute is being deprecated. Instead of providing
-- @Product@, provide @Original@.
--
-- The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
--
-- 'label', 'severity_label' - The severity value of the finding. The allowed values are the following.
--
-- -   @INFORMATIONAL@ - No issue was found.
--
-- -   @LOW@ - The issue does not require action on its own.
--
-- -   @MEDIUM@ - The issue must be addressed but not urgently.
--
-- -   @HIGH@ - The issue must be addressed as a priority.
--
-- -   @CRITICAL@ - The issue must be remediated immediately to avoid it
--     escalating.
--
-- If you provide @Normalized@ and do not provide @Label@, then @Label@ is
-- set automatically as follows.
--
-- -   0 - @INFORMATIONAL@
--
-- -   1–39 - @LOW@
--
-- -   40–69 - @MEDIUM@
--
-- -   70–89 - @HIGH@
--
-- -   90–100 - @CRITICAL@
--
-- 'normalized', 'severity_normalized' - Deprecated. The normalized severity of a finding. This attribute is
-- being deprecated. Instead of providing @Normalized@, provide @Label@.
--
-- If you provide @Label@ and do not provide @Normalized@, then
-- @Normalized@ is set automatically as follows.
--
-- -   @INFORMATIONAL@ - 0
--
-- -   @LOW@ - 1
--
-- -   @MEDIUM@ - 40
--
-- -   @HIGH@ - 70
--
-- -   @CRITICAL@ - 90
--
-- 'original', 'severity_original' - The native severity from the finding product that generated the finding.
newSeverity ::
  Severity
newSeverity =
  Severity'
    { product = Prelude.Nothing,
      label = Prelude.Nothing,
      normalized = Prelude.Nothing,
      original = Prelude.Nothing
    }

-- | Deprecated. This attribute is being deprecated. Instead of providing
-- @Product@, provide @Original@.
--
-- The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
severity_product :: Lens.Lens' Severity (Prelude.Maybe Prelude.Double)
severity_product = Lens.lens (\Severity' {product} -> product) (\s@Severity' {} a -> s {product = a} :: Severity)

-- | The severity value of the finding. The allowed values are the following.
--
-- -   @INFORMATIONAL@ - No issue was found.
--
-- -   @LOW@ - The issue does not require action on its own.
--
-- -   @MEDIUM@ - The issue must be addressed but not urgently.
--
-- -   @HIGH@ - The issue must be addressed as a priority.
--
-- -   @CRITICAL@ - The issue must be remediated immediately to avoid it
--     escalating.
--
-- If you provide @Normalized@ and do not provide @Label@, then @Label@ is
-- set automatically as follows.
--
-- -   0 - @INFORMATIONAL@
--
-- -   1–39 - @LOW@
--
-- -   40–69 - @MEDIUM@
--
-- -   70–89 - @HIGH@
--
-- -   90–100 - @CRITICAL@
severity_label :: Lens.Lens' Severity (Prelude.Maybe SeverityLabel)
severity_label = Lens.lens (\Severity' {label} -> label) (\s@Severity' {} a -> s {label = a} :: Severity)

-- | Deprecated. The normalized severity of a finding. This attribute is
-- being deprecated. Instead of providing @Normalized@, provide @Label@.
--
-- If you provide @Label@ and do not provide @Normalized@, then
-- @Normalized@ is set automatically as follows.
--
-- -   @INFORMATIONAL@ - 0
--
-- -   @LOW@ - 1
--
-- -   @MEDIUM@ - 40
--
-- -   @HIGH@ - 70
--
-- -   @CRITICAL@ - 90
severity_normalized :: Lens.Lens' Severity (Prelude.Maybe Prelude.Int)
severity_normalized = Lens.lens (\Severity' {normalized} -> normalized) (\s@Severity' {} a -> s {normalized = a} :: Severity)

-- | The native severity from the finding product that generated the finding.
severity_original :: Lens.Lens' Severity (Prelude.Maybe Prelude.Text)
severity_original = Lens.lens (\Severity' {original} -> original) (\s@Severity' {} a -> s {original = a} :: Severity)

instance Data.FromJSON Severity where
  parseJSON =
    Data.withObject
      "Severity"
      ( \x ->
          Severity'
            Prelude.<$> (x Data..:? "Product")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Normalized")
            Prelude.<*> (x Data..:? "Original")
      )

instance Prelude.Hashable Severity where
  hashWithSalt _salt Severity' {..} =
    _salt `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` normalized
      `Prelude.hashWithSalt` original

instance Prelude.NFData Severity where
  rnf Severity' {..} =
    Prelude.rnf product
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf normalized
      `Prelude.seq` Prelude.rnf original

instance Data.ToJSON Severity where
  toJSON Severity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Product" Data..=) Prelude.<$> product,
            ("Label" Data..=) Prelude.<$> label,
            ("Normalized" Data..=) Prelude.<$> normalized,
            ("Original" Data..=) Prelude.<$> original
          ]
      )
