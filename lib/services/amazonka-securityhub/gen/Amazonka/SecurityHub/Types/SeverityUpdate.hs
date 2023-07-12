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
-- Module      : Amazonka.SecurityHub.Types.SeverityUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SeverityUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.SeverityLabel

-- | Updates to the severity information for a finding.
--
-- /See:/ 'newSeverityUpdate' smart constructor.
data SeverityUpdate = SeverityUpdate'
  { -- | The severity value of the finding. The allowed values are the following.
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
    label :: Prelude.Maybe SeverityLabel,
    -- | The normalized severity for the finding. This attribute is to be
    -- deprecated in favor of @Label@.
    --
    -- If you provide @Normalized@ and do not provide @Label@, @Label@ is set
    -- automatically as follows.
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
    normalized :: Prelude.Maybe Prelude.Natural,
    -- | The native severity as defined by the Amazon Web Services service or
    -- integrated partner product that generated the finding.
    product :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SeverityUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'severityUpdate_label' - The severity value of the finding. The allowed values are the following.
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
-- 'normalized', 'severityUpdate_normalized' - The normalized severity for the finding. This attribute is to be
-- deprecated in favor of @Label@.
--
-- If you provide @Normalized@ and do not provide @Label@, @Label@ is set
-- automatically as follows.
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
-- 'product', 'severityUpdate_product' - The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
newSeverityUpdate ::
  SeverityUpdate
newSeverityUpdate =
  SeverityUpdate'
    { label = Prelude.Nothing,
      normalized = Prelude.Nothing,
      product = Prelude.Nothing
    }

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
severityUpdate_label :: Lens.Lens' SeverityUpdate (Prelude.Maybe SeverityLabel)
severityUpdate_label = Lens.lens (\SeverityUpdate' {label} -> label) (\s@SeverityUpdate' {} a -> s {label = a} :: SeverityUpdate)

-- | The normalized severity for the finding. This attribute is to be
-- deprecated in favor of @Label@.
--
-- If you provide @Normalized@ and do not provide @Label@, @Label@ is set
-- automatically as follows.
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
severityUpdate_normalized :: Lens.Lens' SeverityUpdate (Prelude.Maybe Prelude.Natural)
severityUpdate_normalized = Lens.lens (\SeverityUpdate' {normalized} -> normalized) (\s@SeverityUpdate' {} a -> s {normalized = a} :: SeverityUpdate)

-- | The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
severityUpdate_product :: Lens.Lens' SeverityUpdate (Prelude.Maybe Prelude.Double)
severityUpdate_product = Lens.lens (\SeverityUpdate' {product} -> product) (\s@SeverityUpdate' {} a -> s {product = a} :: SeverityUpdate)

instance Prelude.Hashable SeverityUpdate where
  hashWithSalt _salt SeverityUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` normalized
      `Prelude.hashWithSalt` product

instance Prelude.NFData SeverityUpdate where
  rnf SeverityUpdate' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf normalized
      `Prelude.seq` Prelude.rnf product

instance Data.ToJSON SeverityUpdate where
  toJSON SeverityUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Label" Data..=) Prelude.<$> label,
            ("Normalized" Data..=) Prelude.<$> normalized,
            ("Product" Data..=) Prelude.<$> product
          ]
      )
