{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SchemaExtensionInfo
  ( SchemaExtensionInfo (..),

    -- * Smart constructor
    mkSchemaExtensionInfo,

    -- * Lenses
    seiDirectoryId,
    seiSchemaExtensionId,
    seiSchemaExtensionStatusReason,
    seiSchemaExtensionStatus,
    seiDescription,
    seiEndDateTime,
    seiStartDateTime,
  )
where

import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a schema extension.
--
-- /See:/ 'mkSchemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { directoryId ::
      Lude.Maybe Lude.Text,
    schemaExtensionId :: Lude.Maybe Lude.Text,
    schemaExtensionStatusReason :: Lude.Maybe Lude.Text,
    schemaExtensionStatus ::
      Lude.Maybe SchemaExtensionStatus,
    description :: Lude.Maybe Lude.Text,
    endDateTime :: Lude.Maybe Lude.Timestamp,
    startDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaExtensionInfo' with the minimum fields required to make a request.
--
-- * 'description' - A description of the schema extension.
-- * 'directoryId' - The identifier of the directory to which the schema extension is applied.
-- * 'endDateTime' - The date and time that the schema extension was completed.
-- * 'schemaExtensionId' - The identifier of the schema extension.
-- * 'schemaExtensionStatus' - The current status of the schema extension.
-- * 'schemaExtensionStatusReason' - The reason for the @SchemaExtensionStatus@ .
-- * 'startDateTime' - The date and time that the schema extension started being applied to the directory.
mkSchemaExtensionInfo ::
  SchemaExtensionInfo
mkSchemaExtensionInfo =
  SchemaExtensionInfo'
    { directoryId = Lude.Nothing,
      schemaExtensionId = Lude.Nothing,
      schemaExtensionStatusReason = Lude.Nothing,
      schemaExtensionStatus = Lude.Nothing,
      description = Lude.Nothing,
      endDateTime = Lude.Nothing,
      startDateTime = Lude.Nothing
    }

-- | The identifier of the directory to which the schema extension is applied.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiDirectoryId :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Text)
seiDirectoryId = Lens.lens (directoryId :: SchemaExtensionInfo -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the schema extension.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionId :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Text)
seiSchemaExtensionId = Lens.lens (schemaExtensionId :: SchemaExtensionInfo -> Lude.Maybe Lude.Text) (\s a -> s {schemaExtensionId = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiSchemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead." #-}

-- | The reason for the @SchemaExtensionStatus@ .
--
-- /Note:/ Consider using 'schemaExtensionStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionStatusReason :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Text)
seiSchemaExtensionStatusReason = Lens.lens (schemaExtensionStatusReason :: SchemaExtensionInfo -> Lude.Maybe Lude.Text) (\s a -> s {schemaExtensionStatusReason = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiSchemaExtensionStatusReason "Use generic-lens or generic-optics with 'schemaExtensionStatusReason' instead." #-}

-- | The current status of the schema extension.
--
-- /Note:/ Consider using 'schemaExtensionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionStatus :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe SchemaExtensionStatus)
seiSchemaExtensionStatus = Lens.lens (schemaExtensionStatus :: SchemaExtensionInfo -> Lude.Maybe SchemaExtensionStatus) (\s a -> s {schemaExtensionStatus = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiSchemaExtensionStatus "Use generic-lens or generic-optics with 'schemaExtensionStatus' instead." #-}

-- | A description of the schema extension.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiDescription :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Text)
seiDescription = Lens.lens (description :: SchemaExtensionInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time that the schema extension was completed.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiEndDateTime :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Timestamp)
seiEndDateTime = Lens.lens (endDateTime :: SchemaExtensionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDateTime = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiEndDateTime "Use generic-lens or generic-optics with 'endDateTime' instead." #-}

-- | The date and time that the schema extension started being applied to the directory.
--
-- /Note:/ Consider using 'startDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiStartDateTime :: Lens.Lens' SchemaExtensionInfo (Lude.Maybe Lude.Timestamp)
seiStartDateTime = Lens.lens (startDateTime :: SchemaExtensionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {startDateTime = a} :: SchemaExtensionInfo)
{-# DEPRECATED seiStartDateTime "Use generic-lens or generic-optics with 'startDateTime' instead." #-}

instance Lude.FromJSON SchemaExtensionInfo where
  parseJSON =
    Lude.withObject
      "SchemaExtensionInfo"
      ( \x ->
          SchemaExtensionInfo'
            Lude.<$> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "SchemaExtensionId")
            Lude.<*> (x Lude..:? "SchemaExtensionStatusReason")
            Lude.<*> (x Lude..:? "SchemaExtensionStatus")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "EndDateTime")
            Lude.<*> (x Lude..:? "StartDateTime")
      )
