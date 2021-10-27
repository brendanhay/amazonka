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
-- Module      : Network.AWS.SecurityHub.Types.SeverityUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.SeverityUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.SeverityLabel

-- | Updates to the severity information for a finding.
--
-- /See:/ 'newSeverityUpdate' smart constructor.
data SeverityUpdate = SeverityUpdate'
  { -- | The native severity as defined by the Amazon Web Services service or
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
    normalized :: Prelude.Maybe Prelude.Natural
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
-- 'product', 'severityUpdate_product' - The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
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
newSeverityUpdate ::
  SeverityUpdate
newSeverityUpdate =
  SeverityUpdate'
    { product = Prelude.Nothing,
      label = Prelude.Nothing,
      normalized = Prelude.Nothing
    }

-- | The native severity as defined by the Amazon Web Services service or
-- integrated partner product that generated the finding.
severityUpdate_product :: Lens.Lens' SeverityUpdate (Prelude.Maybe Prelude.Double)
severityUpdate_product = Lens.lens (\SeverityUpdate' {product} -> product) (\s@SeverityUpdate' {} a -> s {product = a} :: SeverityUpdate)

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

instance Prelude.Hashable SeverityUpdate

instance Prelude.NFData SeverityUpdate

instance Core.ToJSON SeverityUpdate where
  toJSON SeverityUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Product" Core..=) Prelude.<$> product,
            ("Label" Core..=) Prelude.<$> label,
            ("Normalized" Core..=) Prelude.<$> normalized
          ]
      )
