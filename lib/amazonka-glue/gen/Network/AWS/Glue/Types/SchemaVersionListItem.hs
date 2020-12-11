-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionListItem
  ( SchemaVersionListItem (..),

    -- * Smart constructor
    mkSchemaVersionListItem,

    -- * Lenses
    svliStatus,
    svliCreatedTime,
    svliSchemaVersionId,
    svliVersionNumber,
    svliSchemaARN,
  )
where

import Network.AWS.Glue.Types.SchemaVersionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object containing the details about a schema version.
--
-- /See:/ 'mkSchemaVersionListItem' smart constructor.
data SchemaVersionListItem = SchemaVersionListItem'
  { status ::
      Lude.Maybe SchemaVersionStatus,
    createdTime :: Lude.Maybe Lude.Text,
    schemaVersionId :: Lude.Maybe Lude.Text,
    versionNumber :: Lude.Maybe Lude.Natural,
    schemaARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaVersionListItem' with the minimum fields required to make a request.
--
-- * 'createdTime' - The date and time the schema version was created.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'schemaVersionId' - The unique identifier of the schema version.
-- * 'status' - The status of the schema version.
-- * 'versionNumber' - The version number of the schema.
mkSchemaVersionListItem ::
  SchemaVersionListItem
mkSchemaVersionListItem =
  SchemaVersionListItem'
    { status = Lude.Nothing,
      createdTime = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      versionNumber = Lude.Nothing,
      schemaARN = Lude.Nothing
    }

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliStatus :: Lens.Lens' SchemaVersionListItem (Lude.Maybe SchemaVersionStatus)
svliStatus = Lens.lens (status :: SchemaVersionListItem -> Lude.Maybe SchemaVersionStatus) (\s a -> s {status = a} :: SchemaVersionListItem)
{-# DEPRECATED svliStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the schema version was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliCreatedTime :: Lens.Lens' SchemaVersionListItem (Lude.Maybe Lude.Text)
svliCreatedTime = Lens.lens (createdTime :: SchemaVersionListItem -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: SchemaVersionListItem)
{-# DEPRECATED svliCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The unique identifier of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliSchemaVersionId :: Lens.Lens' SchemaVersionListItem (Lude.Maybe Lude.Text)
svliSchemaVersionId = Lens.lens (schemaVersionId :: SchemaVersionListItem -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: SchemaVersionListItem)
{-# DEPRECATED svliSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliVersionNumber :: Lens.Lens' SchemaVersionListItem (Lude.Maybe Lude.Natural)
svliVersionNumber = Lens.lens (versionNumber :: SchemaVersionListItem -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: SchemaVersionListItem)
{-# DEPRECATED svliVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliSchemaARN :: Lens.Lens' SchemaVersionListItem (Lude.Maybe Lude.Text)
svliSchemaARN = Lens.lens (schemaARN :: SchemaVersionListItem -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: SchemaVersionListItem)
{-# DEPRECATED svliSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.FromJSON SchemaVersionListItem where
  parseJSON =
    Lude.withObject
      "SchemaVersionListItem"
      ( \x ->
          SchemaVersionListItem'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "SchemaVersionId")
            Lude.<*> (x Lude..:? "VersionNumber")
            Lude.<*> (x Lude..:? "SchemaArn")
      )
