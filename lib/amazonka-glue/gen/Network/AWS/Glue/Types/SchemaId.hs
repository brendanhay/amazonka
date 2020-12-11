-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaId
  ( SchemaId (..),

    -- * Smart constructor
    mkSchemaId,

    -- * Lenses
    siRegistryName,
    siSchemaName,
    siSchemaARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkSchemaId' smart constructor.
data SchemaId = SchemaId'
  { registryName :: Lude.Maybe Lude.Text,
    schemaName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SchemaId' with the minimum fields required to make a request.
--
-- * 'registryName' - Undocumented field.
-- * 'schemaARN' - Undocumented field.
-- * 'schemaName' - Undocumented field.
mkSchemaId ::
  SchemaId
mkSchemaId =
  SchemaId'
    { registryName = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegistryName :: Lens.Lens' SchemaId (Lude.Maybe Lude.Text)
siRegistryName = Lens.lens (registryName :: SchemaId -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: SchemaId)
{-# DEPRECATED siRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSchemaName :: Lens.Lens' SchemaId (Lude.Maybe Lude.Text)
siSchemaName = Lens.lens (schemaName :: SchemaId -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: SchemaId)
{-# DEPRECATED siSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSchemaARN :: Lens.Lens' SchemaId (Lude.Maybe Lude.Text)
siSchemaARN = Lens.lens (schemaARN :: SchemaId -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: SchemaId)
{-# DEPRECATED siSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.FromJSON SchemaId where
  parseJSON =
    Lude.withObject
      "SchemaId"
      ( \x ->
          SchemaId'
            Lude.<$> (x Lude..:? "RegistryName")
            Lude.<*> (x Lude..:? "SchemaName")
            Lude.<*> (x Lude..:? "SchemaArn")
      )

instance Lude.ToJSON SchemaId where
  toJSON SchemaId' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegistryName" Lude..=) Lude.<$> registryName,
            ("SchemaName" Lude..=) Lude.<$> schemaName,
            ("SchemaArn" Lude..=) Lude.<$> schemaARN
          ]
      )
