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
-- Module      : Amazonka.CodeStarNotifications.Types.TargetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.TargetSummary where

import Amazonka.CodeStarNotifications.Types.TargetStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the targets specified for a notification rule.
--
-- /See:/ 'newTargetSummary' smart constructor.
data TargetSummary = TargetSummary'
  { -- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
    targetAddress :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The type of the target (for example, @SNS@).
    --
    -- -   Chatbot topics are specified as @SNS@.
    --
    -- -   Chatbot clients are specified as @AWSChatbotSlack@.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The status of the target.
    targetStatus :: Prelude.Maybe TargetStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetAddress', 'targetSummary_targetAddress' - The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
--
-- 'targetType', 'targetSummary_targetType' - The type of the target (for example, @SNS@).
--
-- -   Chatbot topics are specified as @SNS@.
--
-- -   Chatbot clients are specified as @AWSChatbotSlack@.
--
-- 'targetStatus', 'targetSummary_targetStatus' - The status of the target.
newTargetSummary ::
  TargetSummary
newTargetSummary =
  TargetSummary'
    { targetAddress = Prelude.Nothing,
      targetType = Prelude.Nothing,
      targetStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
targetSummary_targetAddress :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_targetAddress = Lens.lens (\TargetSummary' {targetAddress} -> targetAddress) (\s@TargetSummary' {} a -> s {targetAddress = a} :: TargetSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The type of the target (for example, @SNS@).
--
-- -   Chatbot topics are specified as @SNS@.
--
-- -   Chatbot clients are specified as @AWSChatbotSlack@.
targetSummary_targetType :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_targetType = Lens.lens (\TargetSummary' {targetType} -> targetType) (\s@TargetSummary' {} a -> s {targetType = a} :: TargetSummary)

-- | The status of the target.
targetSummary_targetStatus :: Lens.Lens' TargetSummary (Prelude.Maybe TargetStatus)
targetSummary_targetStatus = Lens.lens (\TargetSummary' {targetStatus} -> targetStatus) (\s@TargetSummary' {} a -> s {targetStatus = a} :: TargetSummary)

instance Core.FromJSON TargetSummary where
  parseJSON =
    Core.withObject
      "TargetSummary"
      ( \x ->
          TargetSummary'
            Prelude.<$> (x Core..:? "TargetAddress")
            Prelude.<*> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "TargetStatus")
      )

instance Prelude.Hashable TargetSummary where
  hashWithSalt _salt TargetSummary' {..} =
    _salt `Prelude.hashWithSalt` targetAddress
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData TargetSummary where
  rnf TargetSummary' {..} =
    Prelude.rnf targetAddress
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf targetStatus
