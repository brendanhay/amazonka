{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTask
  ( ConversionTask (..),

    -- * Smart constructor
    mkConversionTask,

    -- * Lenses
    ctImportInstance,
    ctState,
    ctStatusMessage,
    ctImportVolume,
    ctConversionTaskId,
    ctExpirationTime,
    ctTags,
  )
where

import Network.AWS.EC2.Types.ConversionTaskState
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a conversion task.
--
-- /See:/ 'mkConversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { importInstance ::
      Lude.Maybe ImportInstanceTaskDetails,
    state :: Lude.Maybe ConversionTaskState,
    statusMessage :: Lude.Maybe Lude.Text,
    importVolume :: Lude.Maybe ImportVolumeTaskDetails,
    conversionTaskId :: Lude.Maybe Lude.Text,
    expirationTime :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConversionTask' with the minimum fields required to make a request.
--
-- * 'conversionTaskId' - The ID of the conversion task.
-- * 'expirationTime' - The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
-- * 'importInstance' - If the task is for importing an instance, this contains information about the import instance task.
-- * 'importVolume' - If the task is for importing a volume, this contains information about the import volume task.
-- * 'state' - The state of the conversion task.
-- * 'statusMessage' - The status message related to the conversion task.
-- * 'tags' - Any tags assigned to the task.
mkConversionTask ::
  ConversionTask
mkConversionTask =
  ConversionTask'
    { importInstance = Lude.Nothing,
      state = Lude.Nothing,
      statusMessage = Lude.Nothing,
      importVolume = Lude.Nothing,
      conversionTaskId = Lude.Nothing,
      expirationTime = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | If the task is for importing an instance, this contains information about the import instance task.
--
-- /Note:/ Consider using 'importInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctImportInstance :: Lens.Lens' ConversionTask (Lude.Maybe ImportInstanceTaskDetails)
ctImportInstance = Lens.lens (importInstance :: ConversionTask -> Lude.Maybe ImportInstanceTaskDetails) (\s a -> s {importInstance = a} :: ConversionTask)
{-# DEPRECATED ctImportInstance "Use generic-lens or generic-optics with 'importInstance' instead." #-}

-- | The state of the conversion task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctState :: Lens.Lens' ConversionTask (Lude.Maybe ConversionTaskState)
ctState = Lens.lens (state :: ConversionTask -> Lude.Maybe ConversionTaskState) (\s a -> s {state = a} :: ConversionTask)
{-# DEPRECATED ctState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message related to the conversion task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctStatusMessage :: Lens.Lens' ConversionTask (Lude.Maybe Lude.Text)
ctStatusMessage = Lens.lens (statusMessage :: ConversionTask -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ConversionTask)
{-# DEPRECATED ctStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | If the task is for importing a volume, this contains information about the import volume task.
--
-- /Note:/ Consider using 'importVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctImportVolume :: Lens.Lens' ConversionTask (Lude.Maybe ImportVolumeTaskDetails)
ctImportVolume = Lens.lens (importVolume :: ConversionTask -> Lude.Maybe ImportVolumeTaskDetails) (\s a -> s {importVolume = a} :: ConversionTask)
{-# DEPRECATED ctImportVolume "Use generic-lens or generic-optics with 'importVolume' instead." #-}

-- | The ID of the conversion task.
--
-- /Note:/ Consider using 'conversionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConversionTaskId :: Lens.Lens' ConversionTask (Lude.Maybe Lude.Text)
ctConversionTaskId = Lens.lens (conversionTaskId :: ConversionTask -> Lude.Maybe Lude.Text) (\s a -> s {conversionTaskId = a} :: ConversionTask)
{-# DEPRECATED ctConversionTaskId "Use generic-lens or generic-optics with 'conversionTaskId' instead." #-}

-- | The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctExpirationTime :: Lens.Lens' ConversionTask (Lude.Maybe Lude.Text)
ctExpirationTime = Lens.lens (expirationTime :: ConversionTask -> Lude.Maybe Lude.Text) (\s a -> s {expirationTime = a} :: ConversionTask)
{-# DEPRECATED ctExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | Any tags assigned to the task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' ConversionTask (Lude.Maybe [Tag])
ctTags = Lens.lens (tags :: ConversionTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ConversionTask)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ConversionTask where
  parseXML x =
    ConversionTask'
      Lude.<$> (x Lude..@? "importInstance")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "importVolume")
      Lude.<*> (x Lude..@? "conversionTaskId")
      Lude.<*> (x Lude..@? "expirationTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
