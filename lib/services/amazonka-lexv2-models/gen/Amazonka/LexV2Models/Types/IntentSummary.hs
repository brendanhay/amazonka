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
-- Module      : Amazonka.LexV2Models.Types.IntentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.InputContext
import Amazonka.LexV2Models.Types.OutputContext
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an intent returned by the @ListIntents@
-- operation.
--
-- /See:/ 'newIntentSummary' smart constructor.
data IntentSummary = IntentSummary'
  { -- | The output contexts that are activated when this intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | If this intent is derived from a built-in intent, the name of the parent
    -- intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | The description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the intent. Use this ID to get
    -- detailed information about the intent with the @DescribeIntent@
    -- operation.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The input contexts that must be active for this intent to be considered
    -- for recognition.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The timestamp of the date and time that the intent was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputContexts', 'intentSummary_outputContexts' - The output contexts that are activated when this intent is fulfilled.
--
-- 'parentIntentSignature', 'intentSummary_parentIntentSignature' - If this intent is derived from a built-in intent, the name of the parent
-- intent.
--
-- 'description', 'intentSummary_description' - The description of the intent.
--
-- 'intentId', 'intentSummary_intentId' - The unique identifier assigned to the intent. Use this ID to get
-- detailed information about the intent with the @DescribeIntent@
-- operation.
--
-- 'intentName', 'intentSummary_intentName' - The name of the intent.
--
-- 'inputContexts', 'intentSummary_inputContexts' - The input contexts that must be active for this intent to be considered
-- for recognition.
--
-- 'lastUpdatedDateTime', 'intentSummary_lastUpdatedDateTime' - The timestamp of the date and time that the intent was last updated.
newIntentSummary ::
  IntentSummary
newIntentSummary =
  IntentSummary'
    { outputContexts = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      description = Prelude.Nothing,
      intentId = Prelude.Nothing,
      intentName = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The output contexts that are activated when this intent is fulfilled.
intentSummary_outputContexts :: Lens.Lens' IntentSummary (Prelude.Maybe [OutputContext])
intentSummary_outputContexts = Lens.lens (\IntentSummary' {outputContexts} -> outputContexts) (\s@IntentSummary' {} a -> s {outputContexts = a} :: IntentSummary) Prelude.. Lens.mapping Lens.coerced

-- | If this intent is derived from a built-in intent, the name of the parent
-- intent.
intentSummary_parentIntentSignature :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_parentIntentSignature = Lens.lens (\IntentSummary' {parentIntentSignature} -> parentIntentSignature) (\s@IntentSummary' {} a -> s {parentIntentSignature = a} :: IntentSummary)

-- | The description of the intent.
intentSummary_description :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_description = Lens.lens (\IntentSummary' {description} -> description) (\s@IntentSummary' {} a -> s {description = a} :: IntentSummary)

-- | The unique identifier assigned to the intent. Use this ID to get
-- detailed information about the intent with the @DescribeIntent@
-- operation.
intentSummary_intentId :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_intentId = Lens.lens (\IntentSummary' {intentId} -> intentId) (\s@IntentSummary' {} a -> s {intentId = a} :: IntentSummary)

-- | The name of the intent.
intentSummary_intentName :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_intentName = Lens.lens (\IntentSummary' {intentName} -> intentName) (\s@IntentSummary' {} a -> s {intentName = a} :: IntentSummary)

-- | The input contexts that must be active for this intent to be considered
-- for recognition.
intentSummary_inputContexts :: Lens.Lens' IntentSummary (Prelude.Maybe [InputContext])
intentSummary_inputContexts = Lens.lens (\IntentSummary' {inputContexts} -> inputContexts) (\s@IntentSummary' {} a -> s {inputContexts = a} :: IntentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of the date and time that the intent was last updated.
intentSummary_lastUpdatedDateTime :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.UTCTime)
intentSummary_lastUpdatedDateTime = Lens.lens (\IntentSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@IntentSummary' {} a -> s {lastUpdatedDateTime = a} :: IntentSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON IntentSummary where
  parseJSON =
    Core.withObject
      "IntentSummary"
      ( \x ->
          IntentSummary'
            Prelude.<$> (x Core..:? "outputContexts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "parentIntentSignature")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "intentId")
            Prelude.<*> (x Core..:? "intentName")
            Prelude.<*> (x Core..:? "inputContexts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable IntentSummary where
  hashWithSalt _salt IntentSummary' {..} =
    _salt `Prelude.hashWithSalt` outputContexts
      `Prelude.hashWithSalt` parentIntentSignature
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` intentId
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` inputContexts
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData IntentSummary where
  rnf IntentSummary' {..} =
    Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
