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
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.InputContext
import Amazonka.LexV2Models.Types.OutputContext
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an intent returned by the @ListIntents@
-- operation.
--
-- /See:/ 'newIntentSummary' smart constructor.
data IntentSummary = IntentSummary'
  { -- | The description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | The input contexts that must be active for this intent to be considered
    -- for recognition.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The unique identifier assigned to the intent. Use this ID to get
    -- detailed information about the intent with the @DescribeIntent@
    -- operation.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the date and time that the intent was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The output contexts that are activated when this intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | If this intent is derived from a built-in intent, the name of the parent
    -- intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text
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
-- 'description', 'intentSummary_description' - The description of the intent.
--
-- 'inputContexts', 'intentSummary_inputContexts' - The input contexts that must be active for this intent to be considered
-- for recognition.
--
-- 'intentId', 'intentSummary_intentId' - The unique identifier assigned to the intent. Use this ID to get
-- detailed information about the intent with the @DescribeIntent@
-- operation.
--
-- 'intentName', 'intentSummary_intentName' - The name of the intent.
--
-- 'lastUpdatedDateTime', 'intentSummary_lastUpdatedDateTime' - The timestamp of the date and time that the intent was last updated.
--
-- 'outputContexts', 'intentSummary_outputContexts' - The output contexts that are activated when this intent is fulfilled.
--
-- 'parentIntentSignature', 'intentSummary_parentIntentSignature' - If this intent is derived from a built-in intent, the name of the parent
-- intent.
newIntentSummary ::
  IntentSummary
newIntentSummary =
  IntentSummary'
    { description = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      intentId = Prelude.Nothing,
      intentName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing
    }

-- | The description of the intent.
intentSummary_description :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_description = Lens.lens (\IntentSummary' {description} -> description) (\s@IntentSummary' {} a -> s {description = a} :: IntentSummary)

-- | The input contexts that must be active for this intent to be considered
-- for recognition.
intentSummary_inputContexts :: Lens.Lens' IntentSummary (Prelude.Maybe [InputContext])
intentSummary_inputContexts = Lens.lens (\IntentSummary' {inputContexts} -> inputContexts) (\s@IntentSummary' {} a -> s {inputContexts = a} :: IntentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier assigned to the intent. Use this ID to get
-- detailed information about the intent with the @DescribeIntent@
-- operation.
intentSummary_intentId :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_intentId = Lens.lens (\IntentSummary' {intentId} -> intentId) (\s@IntentSummary' {} a -> s {intentId = a} :: IntentSummary)

-- | The name of the intent.
intentSummary_intentName :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_intentName = Lens.lens (\IntentSummary' {intentName} -> intentName) (\s@IntentSummary' {} a -> s {intentName = a} :: IntentSummary)

-- | The timestamp of the date and time that the intent was last updated.
intentSummary_lastUpdatedDateTime :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.UTCTime)
intentSummary_lastUpdatedDateTime = Lens.lens (\IntentSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@IntentSummary' {} a -> s {lastUpdatedDateTime = a} :: IntentSummary) Prelude.. Lens.mapping Data._Time

-- | The output contexts that are activated when this intent is fulfilled.
intentSummary_outputContexts :: Lens.Lens' IntentSummary (Prelude.Maybe [OutputContext])
intentSummary_outputContexts = Lens.lens (\IntentSummary' {outputContexts} -> outputContexts) (\s@IntentSummary' {} a -> s {outputContexts = a} :: IntentSummary) Prelude.. Lens.mapping Lens.coerced

-- | If this intent is derived from a built-in intent, the name of the parent
-- intent.
intentSummary_parentIntentSignature :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_parentIntentSignature = Lens.lens (\IntentSummary' {parentIntentSignature} -> parentIntentSignature) (\s@IntentSummary' {} a -> s {parentIntentSignature = a} :: IntentSummary)

instance Data.FromJSON IntentSummary where
  parseJSON =
    Data.withObject
      "IntentSummary"
      ( \x ->
          IntentSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "inputContexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "intentId")
            Prelude.<*> (x Data..:? "intentName")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "outputContexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "parentIntentSignature")
      )

instance Prelude.Hashable IntentSummary where
  hashWithSalt _salt IntentSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inputContexts
      `Prelude.hashWithSalt` intentId
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` outputContexts
      `Prelude.hashWithSalt` parentIntentSignature

instance Prelude.NFData IntentSummary where
  rnf IntentSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf parentIntentSignature
