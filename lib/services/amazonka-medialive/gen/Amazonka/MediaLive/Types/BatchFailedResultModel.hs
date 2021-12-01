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
-- Module      : Amazonka.MediaLive.Types.BatchFailedResultModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BatchFailedResultModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details from a failed operation
--
-- /See:/ 'newBatchFailedResultModel' smart constructor.
data BatchFailedResultModel = BatchFailedResultModel'
  { -- | ARN of the resource
    arn :: Prelude.Maybe Prelude.Text,
    -- | ID of the resource
    id :: Prelude.Maybe Prelude.Text,
    -- | Error code for the failed operation
    code :: Prelude.Maybe Prelude.Text,
    -- | Error message for the failed operation
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchFailedResultModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'batchFailedResultModel_arn' - ARN of the resource
--
-- 'id', 'batchFailedResultModel_id' - ID of the resource
--
-- 'code', 'batchFailedResultModel_code' - Error code for the failed operation
--
-- 'message', 'batchFailedResultModel_message' - Error message for the failed operation
newBatchFailedResultModel ::
  BatchFailedResultModel
newBatchFailedResultModel =
  BatchFailedResultModel'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | ARN of the resource
batchFailedResultModel_arn :: Lens.Lens' BatchFailedResultModel (Prelude.Maybe Prelude.Text)
batchFailedResultModel_arn = Lens.lens (\BatchFailedResultModel' {arn} -> arn) (\s@BatchFailedResultModel' {} a -> s {arn = a} :: BatchFailedResultModel)

-- | ID of the resource
batchFailedResultModel_id :: Lens.Lens' BatchFailedResultModel (Prelude.Maybe Prelude.Text)
batchFailedResultModel_id = Lens.lens (\BatchFailedResultModel' {id} -> id) (\s@BatchFailedResultModel' {} a -> s {id = a} :: BatchFailedResultModel)

-- | Error code for the failed operation
batchFailedResultModel_code :: Lens.Lens' BatchFailedResultModel (Prelude.Maybe Prelude.Text)
batchFailedResultModel_code = Lens.lens (\BatchFailedResultModel' {code} -> code) (\s@BatchFailedResultModel' {} a -> s {code = a} :: BatchFailedResultModel)

-- | Error message for the failed operation
batchFailedResultModel_message :: Lens.Lens' BatchFailedResultModel (Prelude.Maybe Prelude.Text)
batchFailedResultModel_message = Lens.lens (\BatchFailedResultModel' {message} -> message) (\s@BatchFailedResultModel' {} a -> s {message = a} :: BatchFailedResultModel)

instance Core.FromJSON BatchFailedResultModel where
  parseJSON =
    Core.withObject
      "BatchFailedResultModel"
      ( \x ->
          BatchFailedResultModel'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "code")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable BatchFailedResultModel where
  hashWithSalt salt' BatchFailedResultModel' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn

instance Prelude.NFData BatchFailedResultModel where
  rnf BatchFailedResultModel' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf id
