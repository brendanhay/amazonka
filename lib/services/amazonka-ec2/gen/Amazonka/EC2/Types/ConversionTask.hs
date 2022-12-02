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
-- Module      : Amazonka.EC2.Types.ConversionTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ConversionTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ConversionTaskState
import Amazonka.EC2.Types.ImportInstanceTaskDetails
import Amazonka.EC2.Types.ImportVolumeTaskDetails
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a conversion task.
--
-- /See:/ 'newConversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { -- | Any tags assigned to the task.
    tags :: Prelude.Maybe [Tag],
    -- | If the task is for importing an instance, this contains information
    -- about the import instance task.
    importInstance :: Prelude.Maybe ImportInstanceTaskDetails,
    -- | The ID of the conversion task.
    conversionTaskId :: Prelude.Maybe Prelude.Text,
    -- | The time when the task expires. If the upload isn\'t complete before the
    -- expiration time, we automatically cancel the task.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | The state of the conversion task.
    state :: Prelude.Maybe ConversionTaskState,
    -- | The status message related to the conversion task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | If the task is for importing a volume, this contains information about
    -- the import volume task.
    importVolume :: Prelude.Maybe ImportVolumeTaskDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'conversionTask_tags' - Any tags assigned to the task.
--
-- 'importInstance', 'conversionTask_importInstance' - If the task is for importing an instance, this contains information
-- about the import instance task.
--
-- 'conversionTaskId', 'conversionTask_conversionTaskId' - The ID of the conversion task.
--
-- 'expirationTime', 'conversionTask_expirationTime' - The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
--
-- 'state', 'conversionTask_state' - The state of the conversion task.
--
-- 'statusMessage', 'conversionTask_statusMessage' - The status message related to the conversion task.
--
-- 'importVolume', 'conversionTask_importVolume' - If the task is for importing a volume, this contains information about
-- the import volume task.
newConversionTask ::
  ConversionTask
newConversionTask =
  ConversionTask'
    { tags = Prelude.Nothing,
      importInstance = Prelude.Nothing,
      conversionTaskId = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      importVolume = Prelude.Nothing
    }

-- | Any tags assigned to the task.
conversionTask_tags :: Lens.Lens' ConversionTask (Prelude.Maybe [Tag])
conversionTask_tags = Lens.lens (\ConversionTask' {tags} -> tags) (\s@ConversionTask' {} a -> s {tags = a} :: ConversionTask) Prelude.. Lens.mapping Lens.coerced

-- | If the task is for importing an instance, this contains information
-- about the import instance task.
conversionTask_importInstance :: Lens.Lens' ConversionTask (Prelude.Maybe ImportInstanceTaskDetails)
conversionTask_importInstance = Lens.lens (\ConversionTask' {importInstance} -> importInstance) (\s@ConversionTask' {} a -> s {importInstance = a} :: ConversionTask)

-- | The ID of the conversion task.
conversionTask_conversionTaskId :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_conversionTaskId = Lens.lens (\ConversionTask' {conversionTaskId} -> conversionTaskId) (\s@ConversionTask' {} a -> s {conversionTaskId = a} :: ConversionTask)

-- | The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
conversionTask_expirationTime :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_expirationTime = Lens.lens (\ConversionTask' {expirationTime} -> expirationTime) (\s@ConversionTask' {} a -> s {expirationTime = a} :: ConversionTask)

-- | The state of the conversion task.
conversionTask_state :: Lens.Lens' ConversionTask (Prelude.Maybe ConversionTaskState)
conversionTask_state = Lens.lens (\ConversionTask' {state} -> state) (\s@ConversionTask' {} a -> s {state = a} :: ConversionTask)

-- | The status message related to the conversion task.
conversionTask_statusMessage :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_statusMessage = Lens.lens (\ConversionTask' {statusMessage} -> statusMessage) (\s@ConversionTask' {} a -> s {statusMessage = a} :: ConversionTask)

-- | If the task is for importing a volume, this contains information about
-- the import volume task.
conversionTask_importVolume :: Lens.Lens' ConversionTask (Prelude.Maybe ImportVolumeTaskDetails)
conversionTask_importVolume = Lens.lens (\ConversionTask' {importVolume} -> importVolume) (\s@ConversionTask' {} a -> s {importVolume = a} :: ConversionTask)

instance Data.FromXML ConversionTask where
  parseXML x =
    ConversionTask'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "importInstance")
      Prelude.<*> (x Data..@? "conversionTaskId")
      Prelude.<*> (x Data..@? "expirationTime")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "importVolume")

instance Prelude.Hashable ConversionTask where
  hashWithSalt _salt ConversionTask' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` importInstance
      `Prelude.hashWithSalt` conversionTaskId
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` importVolume

instance Prelude.NFData ConversionTask where
  rnf ConversionTask' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf importInstance
      `Prelude.seq` Prelude.rnf conversionTaskId
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf importVolume
