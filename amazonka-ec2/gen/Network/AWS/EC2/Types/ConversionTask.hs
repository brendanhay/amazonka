{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ConversionTaskState
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a conversion task.
--
-- /See:/ 'newConversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { -- | The status message related to the conversion task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | If the task is for importing an instance, this contains information
    -- about the import instance task.
    importInstance :: Prelude.Maybe ImportInstanceTaskDetails,
    -- | The time when the task expires. If the upload isn\'t complete before the
    -- expiration time, we automatically cancel the task.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | If the task is for importing a volume, this contains information about
    -- the import volume task.
    importVolume :: Prelude.Maybe ImportVolumeTaskDetails,
    -- | The state of the conversion task.
    state :: Prelude.Maybe ConversionTaskState,
    -- | Any tags assigned to the task.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the conversion task.
    conversionTaskId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      importInstance = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      importVolume = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      conversionTaskId = Prelude.Nothing
    }

-- | The status message related to the conversion task.
conversionTask_statusMessage :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_statusMessage = Lens.lens (\ConversionTask' {statusMessage} -> statusMessage) (\s@ConversionTask' {} a -> s {statusMessage = a} :: ConversionTask)

-- | If the task is for importing an instance, this contains information
-- about the import instance task.
conversionTask_importInstance :: Lens.Lens' ConversionTask (Prelude.Maybe ImportInstanceTaskDetails)
conversionTask_importInstance = Lens.lens (\ConversionTask' {importInstance} -> importInstance) (\s@ConversionTask' {} a -> s {importInstance = a} :: ConversionTask)

-- | The time when the task expires. If the upload isn\'t complete before the
-- expiration time, we automatically cancel the task.
conversionTask_expirationTime :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_expirationTime = Lens.lens (\ConversionTask' {expirationTime} -> expirationTime) (\s@ConversionTask' {} a -> s {expirationTime = a} :: ConversionTask)

-- | If the task is for importing a volume, this contains information about
-- the import volume task.
conversionTask_importVolume :: Lens.Lens' ConversionTask (Prelude.Maybe ImportVolumeTaskDetails)
conversionTask_importVolume = Lens.lens (\ConversionTask' {importVolume} -> importVolume) (\s@ConversionTask' {} a -> s {importVolume = a} :: ConversionTask)

-- | The state of the conversion task.
conversionTask_state :: Lens.Lens' ConversionTask (Prelude.Maybe ConversionTaskState)
conversionTask_state = Lens.lens (\ConversionTask' {state} -> state) (\s@ConversionTask' {} a -> s {state = a} :: ConversionTask)

-- | Any tags assigned to the task.
conversionTask_tags :: Lens.Lens' ConversionTask (Prelude.Maybe [Tag])
conversionTask_tags = Lens.lens (\ConversionTask' {tags} -> tags) (\s@ConversionTask' {} a -> s {tags = a} :: ConversionTask) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the conversion task.
conversionTask_conversionTaskId :: Lens.Lens' ConversionTask (Prelude.Maybe Prelude.Text)
conversionTask_conversionTaskId = Lens.lens (\ConversionTask' {conversionTaskId} -> conversionTaskId) (\s@ConversionTask' {} a -> s {conversionTaskId = a} :: ConversionTask)

instance Prelude.FromXML ConversionTask where
  parseXML x =
    ConversionTask'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "importInstance")
      Prelude.<*> (x Prelude..@? "expirationTime")
      Prelude.<*> (x Prelude..@? "importVolume")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "conversionTaskId")

instance Prelude.Hashable ConversionTask

instance Prelude.NFData ConversionTask
