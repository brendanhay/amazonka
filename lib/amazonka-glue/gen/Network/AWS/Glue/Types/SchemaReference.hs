{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaReference
  ( SchemaReference (..),

    -- * Smart constructor
    mkSchemaReference,

    -- * Lenses
    srSchemaVersionId,
    srSchemaId,
    srSchemaVersionNumber,
  )
where

import Network.AWS.Glue.Types.SchemaId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that references a schema stored in the AWS Glue Schema Registry.
--
-- /See:/ 'mkSchemaReference' smart constructor.
data SchemaReference = SchemaReference'
  { -- | The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
    schemaId :: Lude.Maybe SchemaId,
    -- | The version number of the schema.
    schemaVersionNumber :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaReference' with the minimum fields required to make a request.
--
-- * 'schemaVersionId' - The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
-- * 'schemaId' - A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
-- * 'schemaVersionNumber' - The version number of the schema.
mkSchemaReference ::
  SchemaReference
mkSchemaReference =
  SchemaReference'
    { schemaVersionId = Lude.Nothing,
      schemaId = Lude.Nothing,
      schemaVersionNumber = Lude.Nothing
    }

-- | The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaVersionId :: Lens.Lens' SchemaReference (Lude.Maybe Lude.Text)
srSchemaVersionId = Lens.lens (schemaVersionId :: SchemaReference -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: SchemaReference)
{-# DEPRECATED srSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaId :: Lens.Lens' SchemaReference (Lude.Maybe SchemaId)
srSchemaId = Lens.lens (schemaId :: SchemaReference -> Lude.Maybe SchemaId) (\s a -> s {schemaId = a} :: SchemaReference)
{-# DEPRECATED srSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaVersionNumber :: Lens.Lens' SchemaReference (Lude.Maybe Lude.Natural)
srSchemaVersionNumber = Lens.lens (schemaVersionNumber :: SchemaReference -> Lude.Maybe Lude.Natural) (\s a -> s {schemaVersionNumber = a} :: SchemaReference)
{-# DEPRECATED srSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Lude.FromJSON SchemaReference where
  parseJSON =
    Lude.withObject
      "SchemaReference"
      ( \x ->
          SchemaReference'
            Lude.<$> (x Lude..:? "SchemaVersionId")
            Lude.<*> (x Lude..:? "SchemaId")
            Lude.<*> (x Lude..:? "SchemaVersionNumber")
      )

instance Lude.ToJSON SchemaReference where
  toJSON SchemaReference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaVersionId" Lude..=) Lude.<$> schemaVersionId,
            ("SchemaId" Lude..=) Lude.<$> schemaId,
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber
          ]
      )
