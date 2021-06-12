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
-- Module      : Network.AWS.MediaLive.Types.BatchFailedResultModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchFailedResultModel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details from a failed operation
--
-- /See:/ 'newBatchFailedResultModel' smart constructor.
data BatchFailedResultModel = BatchFailedResultModel'
  { -- | Error message for the failed operation
    message :: Core.Maybe Core.Text,
    -- | ARN of the resource
    arn :: Core.Maybe Core.Text,
    -- | ID of the resource
    id :: Core.Maybe Core.Text,
    -- | Error code for the failed operation
    code :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchFailedResultModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchFailedResultModel_message' - Error message for the failed operation
--
-- 'arn', 'batchFailedResultModel_arn' - ARN of the resource
--
-- 'id', 'batchFailedResultModel_id' - ID of the resource
--
-- 'code', 'batchFailedResultModel_code' - Error code for the failed operation
newBatchFailedResultModel ::
  BatchFailedResultModel
newBatchFailedResultModel =
  BatchFailedResultModel'
    { message = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      code = Core.Nothing
    }

-- | Error message for the failed operation
batchFailedResultModel_message :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
batchFailedResultModel_message = Lens.lens (\BatchFailedResultModel' {message} -> message) (\s@BatchFailedResultModel' {} a -> s {message = a} :: BatchFailedResultModel)

-- | ARN of the resource
batchFailedResultModel_arn :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
batchFailedResultModel_arn = Lens.lens (\BatchFailedResultModel' {arn} -> arn) (\s@BatchFailedResultModel' {} a -> s {arn = a} :: BatchFailedResultModel)

-- | ID of the resource
batchFailedResultModel_id :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
batchFailedResultModel_id = Lens.lens (\BatchFailedResultModel' {id} -> id) (\s@BatchFailedResultModel' {} a -> s {id = a} :: BatchFailedResultModel)

-- | Error code for the failed operation
batchFailedResultModel_code :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
batchFailedResultModel_code = Lens.lens (\BatchFailedResultModel' {code} -> code) (\s@BatchFailedResultModel' {} a -> s {code = a} :: BatchFailedResultModel)

instance Core.FromJSON BatchFailedResultModel where
  parseJSON =
    Core.withObject
      "BatchFailedResultModel"
      ( \x ->
          BatchFailedResultModel'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "code")
      )

instance Core.Hashable BatchFailedResultModel

instance Core.NFData BatchFailedResultModel
