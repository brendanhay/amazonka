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
-- Module      : Amazonka.LexV2Models.Types.SampleUtterance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SampleUtterance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A sample utterance that invokes an intent or respond to a slot
-- elicitation prompt.
--
-- /See:/ 'newSampleUtterance' smart constructor.
data SampleUtterance = SampleUtterance'
  { -- | The sample utterance that Amazon Lex uses to build its machine-learning
    -- model to recognize intents.
    utterance :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SampleUtterance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'utterance', 'sampleUtterance_utterance' - The sample utterance that Amazon Lex uses to build its machine-learning
-- model to recognize intents.
newSampleUtterance ::
  -- | 'utterance'
  Prelude.Text ->
  SampleUtterance
newSampleUtterance pUtterance_ =
  SampleUtterance' {utterance = pUtterance_}

-- | The sample utterance that Amazon Lex uses to build its machine-learning
-- model to recognize intents.
sampleUtterance_utterance :: Lens.Lens' SampleUtterance Prelude.Text
sampleUtterance_utterance = Lens.lens (\SampleUtterance' {utterance} -> utterance) (\s@SampleUtterance' {} a -> s {utterance = a} :: SampleUtterance)

instance Data.FromJSON SampleUtterance where
  parseJSON =
    Data.withObject
      "SampleUtterance"
      ( \x ->
          SampleUtterance' Prelude.<$> (x Data..: "utterance")
      )

instance Prelude.Hashable SampleUtterance where
  hashWithSalt _salt SampleUtterance' {..} =
    _salt `Prelude.hashWithSalt` utterance

instance Prelude.NFData SampleUtterance where
  rnf SampleUtterance' {..} = Prelude.rnf utterance

instance Data.ToJSON SampleUtterance where
  toJSON SampleUtterance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("utterance" Data..= utterance)]
      )
