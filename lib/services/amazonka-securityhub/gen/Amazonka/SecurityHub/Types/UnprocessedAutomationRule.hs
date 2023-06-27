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
-- Module      : Amazonka.SecurityHub.Types.UnprocessedAutomationRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UnprocessedAutomationRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- process and why.
--
-- /See:/ 'newUnprocessedAutomationRule' smart constructor.
data UnprocessedAutomationRule = UnprocessedAutomationRule'
  { -- | The error code associated with the unprocessed automation rule.
    errorCode :: Prelude.Maybe Prelude.Int,
    -- | An error message describing why a request didn\'t process a specific
    -- rule.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the unprocessed automation rule.
    ruleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedAutomationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedAutomationRule_errorCode' - The error code associated with the unprocessed automation rule.
--
-- 'errorMessage', 'unprocessedAutomationRule_errorMessage' - An error message describing why a request didn\'t process a specific
-- rule.
--
-- 'ruleArn', 'unprocessedAutomationRule_ruleArn' - The Amazon Resource Name (ARN) for the unprocessed automation rule.
newUnprocessedAutomationRule ::
  UnprocessedAutomationRule
newUnprocessedAutomationRule =
  UnprocessedAutomationRule'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      ruleArn = Prelude.Nothing
    }

-- | The error code associated with the unprocessed automation rule.
unprocessedAutomationRule_errorCode :: Lens.Lens' UnprocessedAutomationRule (Prelude.Maybe Prelude.Int)
unprocessedAutomationRule_errorCode = Lens.lens (\UnprocessedAutomationRule' {errorCode} -> errorCode) (\s@UnprocessedAutomationRule' {} a -> s {errorCode = a} :: UnprocessedAutomationRule)

-- | An error message describing why a request didn\'t process a specific
-- rule.
unprocessedAutomationRule_errorMessage :: Lens.Lens' UnprocessedAutomationRule (Prelude.Maybe Prelude.Text)
unprocessedAutomationRule_errorMessage = Lens.lens (\UnprocessedAutomationRule' {errorMessage} -> errorMessage) (\s@UnprocessedAutomationRule' {} a -> s {errorMessage = a} :: UnprocessedAutomationRule)

-- | The Amazon Resource Name (ARN) for the unprocessed automation rule.
unprocessedAutomationRule_ruleArn :: Lens.Lens' UnprocessedAutomationRule (Prelude.Maybe Prelude.Text)
unprocessedAutomationRule_ruleArn = Lens.lens (\UnprocessedAutomationRule' {ruleArn} -> ruleArn) (\s@UnprocessedAutomationRule' {} a -> s {ruleArn = a} :: UnprocessedAutomationRule)

instance Data.FromJSON UnprocessedAutomationRule where
  parseJSON =
    Data.withObject
      "UnprocessedAutomationRule"
      ( \x ->
          UnprocessedAutomationRule'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "RuleArn")
      )

instance Prelude.Hashable UnprocessedAutomationRule where
  hashWithSalt _salt UnprocessedAutomationRule' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` ruleArn

instance Prelude.NFData UnprocessedAutomationRule where
  rnf UnprocessedAutomationRule' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf ruleArn
