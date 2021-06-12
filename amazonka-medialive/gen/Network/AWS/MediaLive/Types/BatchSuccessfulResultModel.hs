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
-- Module      : Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchSuccessfulResultModel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details from a successful operation
--
-- /See:/ 'newBatchSuccessfulResultModel' smart constructor.
data BatchSuccessfulResultModel = BatchSuccessfulResultModel'
  { -- | ARN of the resource
    arn :: Core.Maybe Core.Text,
    -- | ID of the resource
    id :: Core.Maybe Core.Text,
    -- | Current state of the resource
    state :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'id', 'batchSuccessfulResultModel_id' - ID of the resource
--
-- 'state', 'batchSuccessfulResultModel_state' - Current state of the resource
newBatchSuccessfulResultModel ::
  BatchSuccessfulResultModel
newBatchSuccessfulResultModel =
  BatchSuccessfulResultModel'
    { arn = Core.Nothing,
      id = Core.Nothing,
      state = Core.Nothing
    }

-- | ARN of the resource
batchSuccessfulResultModel_arn :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
batchSuccessfulResultModel_arn = Lens.lens (\BatchSuccessfulResultModel' {arn} -> arn) (\s@BatchSuccessfulResultModel' {} a -> s {arn = a} :: BatchSuccessfulResultModel)

-- | ID of the resource
batchSuccessfulResultModel_id :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
batchSuccessfulResultModel_id = Lens.lens (\BatchSuccessfulResultModel' {id} -> id) (\s@BatchSuccessfulResultModel' {} a -> s {id = a} :: BatchSuccessfulResultModel)

-- | Current state of the resource
batchSuccessfulResultModel_state :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
batchSuccessfulResultModel_state = Lens.lens (\BatchSuccessfulResultModel' {state} -> state) (\s@BatchSuccessfulResultModel' {} a -> s {state = a} :: BatchSuccessfulResultModel)

instance Core.FromJSON BatchSuccessfulResultModel where
  parseJSON =
    Core.withObject
      "BatchSuccessfulResultModel"
      ( \x ->
          BatchSuccessfulResultModel'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "state")
      )

instance Core.Hashable BatchSuccessfulResultModel

instance Core.NFData BatchSuccessfulResultModel
