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
-- Module      : Network.AWS.EC2.Types.ConversionTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTask where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ConversionTaskState
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a conversion task.
--
-- /See:/ 'newConversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { -- | The status message related to the conversion task.
    statusMessage :: Core.Maybe Core.Text,
    -- | If the task is for importing an instance, this contains information
    -- about the import instance task.
    importInstance :: Core.Maybe ImportInstanceTaskDetails,
    -- | The time when the task expires. If the upload isn\'t complete before the
    -- expiration time, we automatically cancel the task.
    expirationTime :: Core.Maybe Core.Text,
    -- | If the task is for importing a volume, this contains information about
    -- the import volume task.
    importVolume :: Core.Maybe ImportVolumeTaskDetails,
    -- | The state of the conversion task.
    state :: Core.Maybe ConversionTaskState,
    -- | Any tags assigned to the task.
    tags :: Core.Maybe [Tag],
    -- | The ID of the conversion task.
    conversionTaskId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConversionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'conversionTask_statusMessage' - The status message related to the conversion task.
--
-- 'importInstance', 'conversionTask_importInstance' - If the task is for importing an instance, this contains information
-- about the import instance task.
--
-- 'expirationTime', 'conversionTask_expirationTime' - The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
--
-- 'importVolume', 'conversionTask_importVolume' - If the task is for importing a volume, this contains information about
-- the import volume task.
--
-- 'state', 'conversionTask_state' - The state of the conversion task.
--
-- 'tags', 'conversionTask_tags' - Any tags assigned to the task.
--
-- 'conversionTaskId', 'conversionTask_conversionTaskId' - The ID of the conversion task.
newConversionTask ::
  ConversionTask
newConversionTask =
  ConversionTask'
    { statusMessage = Core.Nothing,
      importInstance = Core.Nothing,
      expirationTime = Core.Nothing,
      importVolume = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      conversionTaskId = Core.Nothing
    }

-- | The status message related to the conversion task.
conversionTask_statusMessage :: Lens.Lens' ConversionTask (Core.Maybe Core.Text)
conversionTask_statusMessage = Lens.lens (\ConversionTask' {statusMessage} -> statusMessage) (\s@ConversionTask' {} a -> s {statusMessage = a} :: ConversionTask)

-- | If the task is for importing an instance, this contains information
-- about the import instance task.
conversionTask_importInstance :: Lens.Lens' ConversionTask (Core.Maybe ImportInstanceTaskDetails)
conversionTask_importInstance = Lens.lens (\ConversionTask' {importInstance} -> importInstance) (\s@ConversionTask' {} a -> s {importInstance = a} :: ConversionTask)

-- | The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
conversionTask_expirationTime :: Lens.Lens' ConversionTask (Core.Maybe Core.Text)
conversionTask_expirationTime = Lens.lens (\ConversionTask' {expirationTime} -> expirationTime) (\s@ConversionTask' {} a -> s {expirationTime = a} :: ConversionTask)

-- | If the task is for importing a volume, this contains information about
-- the import volume task.
conversionTask_importVolume :: Lens.Lens' ConversionTask (Core.Maybe ImportVolumeTaskDetails)
conversionTask_importVolume = Lens.lens (\ConversionTask' {importVolume} -> importVolume) (\s@ConversionTask' {} a -> s {importVolume = a} :: ConversionTask)

-- | The state of the conversion task.
conversionTask_state :: Lens.Lens' ConversionTask (Core.Maybe ConversionTaskState)
conversionTask_state = Lens.lens (\ConversionTask' {state} -> state) (\s@ConversionTask' {} a -> s {state = a} :: ConversionTask)

-- | Any tags assigned to the task.
conversionTask_tags :: Lens.Lens' ConversionTask (Core.Maybe [Tag])
conversionTask_tags = Lens.lens (\ConversionTask' {tags} -> tags) (\s@ConversionTask' {} a -> s {tags = a} :: ConversionTask) Core.. Lens.mapping Lens._Coerce

-- | The ID of the conversion task.
conversionTask_conversionTaskId :: Lens.Lens' ConversionTask (Core.Maybe Core.Text)
conversionTask_conversionTaskId = Lens.lens (\ConversionTask' {conversionTaskId} -> conversionTaskId) (\s@ConversionTask' {} a -> s {conversionTaskId = a} :: ConversionTask)

instance Core.FromXML ConversionTask where
  parseXML x =
    ConversionTask'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "importInstance")
      Core.<*> (x Core..@? "expirationTime")
      Core.<*> (x Core..@? "importVolume")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "conversionTaskId")

instance Core.Hashable ConversionTask

instance Core.NFData ConversionTask
