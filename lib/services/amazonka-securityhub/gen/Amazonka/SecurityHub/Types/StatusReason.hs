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
-- Module      : Amazonka.SecurityHub.Types.StatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StatusReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides additional context for the value of @Compliance.Status@.
--
-- /See:/ 'newStatusReason' smart constructor.
data StatusReason = StatusReason'
  { -- | The corresponding description for the status reason code.
    description :: Prelude.Maybe Prelude.Text,
    -- | A code that represents a reason for the control status. For the list of
    -- status reason codes and their meanings, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
    -- in the /Security Hub User Guide/.
    reasonCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'statusReason_description' - The corresponding description for the status reason code.
--
-- 'reasonCode', 'statusReason_reasonCode' - A code that represents a reason for the control status. For the list of
-- status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
newStatusReason ::
  -- | 'reasonCode'
  Prelude.Text ->
  StatusReason
newStatusReason pReasonCode_ =
  StatusReason'
    { description = Prelude.Nothing,
      reasonCode = pReasonCode_
    }

-- | The corresponding description for the status reason code.
statusReason_description :: Lens.Lens' StatusReason (Prelude.Maybe Prelude.Text)
statusReason_description = Lens.lens (\StatusReason' {description} -> description) (\s@StatusReason' {} a -> s {description = a} :: StatusReason)

-- | A code that represents a reason for the control status. For the list of
-- status reason codes and their meanings, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-results.html#securityhub-standards-results-asff Standards-related information in the ASFF>
-- in the /Security Hub User Guide/.
statusReason_reasonCode :: Lens.Lens' StatusReason Prelude.Text
statusReason_reasonCode = Lens.lens (\StatusReason' {reasonCode} -> reasonCode) (\s@StatusReason' {} a -> s {reasonCode = a} :: StatusReason)

instance Core.FromJSON StatusReason where
  parseJSON =
    Core.withObject
      "StatusReason"
      ( \x ->
          StatusReason'
            Prelude.<$> (x Core..:? "Description")
            Prelude.<*> (x Core..: "ReasonCode")
      )

instance Prelude.Hashable StatusReason where
  hashWithSalt _salt StatusReason' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reasonCode

instance Prelude.NFData StatusReason where
  rnf StatusReason' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf reasonCode

instance Core.ToJSON StatusReason where
  toJSON StatusReason' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("ReasonCode" Core..= reasonCode)
          ]
      )
