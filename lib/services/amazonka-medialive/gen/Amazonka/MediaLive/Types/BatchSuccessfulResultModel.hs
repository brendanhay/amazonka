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
-- Module      : Amazonka.MediaLive.Types.BatchSuccessfulResultModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BatchSuccessfulResultModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details from a successful operation
--
-- /See:/ 'newBatchSuccessfulResultModel' smart constructor.
data BatchSuccessfulResultModel = BatchSuccessfulResultModel'
  { -- | ARN of the resource
    arn :: Prelude.Maybe Prelude.Text,
    -- | Current state of the resource
    state :: Prelude.Maybe Prelude.Text,
    -- | ID of the resource
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSuccessfulResultModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'batchSuccessfulResultModel_arn' - ARN of the resource
--
-- 'state', 'batchSuccessfulResultModel_state' - Current state of the resource
--
-- 'id', 'batchSuccessfulResultModel_id' - ID of the resource
newBatchSuccessfulResultModel ::
  BatchSuccessfulResultModel
newBatchSuccessfulResultModel =
  BatchSuccessfulResultModel'
    { arn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | ARN of the resource
batchSuccessfulResultModel_arn :: Lens.Lens' BatchSuccessfulResultModel (Prelude.Maybe Prelude.Text)
batchSuccessfulResultModel_arn = Lens.lens (\BatchSuccessfulResultModel' {arn} -> arn) (\s@BatchSuccessfulResultModel' {} a -> s {arn = a} :: BatchSuccessfulResultModel)

-- | Current state of the resource
batchSuccessfulResultModel_state :: Lens.Lens' BatchSuccessfulResultModel (Prelude.Maybe Prelude.Text)
batchSuccessfulResultModel_state = Lens.lens (\BatchSuccessfulResultModel' {state} -> state) (\s@BatchSuccessfulResultModel' {} a -> s {state = a} :: BatchSuccessfulResultModel)

-- | ID of the resource
batchSuccessfulResultModel_id :: Lens.Lens' BatchSuccessfulResultModel (Prelude.Maybe Prelude.Text)
batchSuccessfulResultModel_id = Lens.lens (\BatchSuccessfulResultModel' {id} -> id) (\s@BatchSuccessfulResultModel' {} a -> s {id = a} :: BatchSuccessfulResultModel)

instance Core.FromJSON BatchSuccessfulResultModel where
  parseJSON =
    Core.withObject
      "BatchSuccessfulResultModel"
      ( \x ->
          BatchSuccessfulResultModel'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable BatchSuccessfulResultModel where
  hashWithSalt _salt BatchSuccessfulResultModel' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id

instance Prelude.NFData BatchSuccessfulResultModel where
  rnf BatchSuccessfulResultModel' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
