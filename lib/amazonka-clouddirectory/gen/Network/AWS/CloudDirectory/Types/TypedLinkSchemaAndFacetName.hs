{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
  ( TypedLinkSchemaAndFacetName (..),

    -- * Smart constructor
    mkTypedLinkSchemaAndFacetName,

    -- * Lenses
    tlsafnSchemaARN,
    tlsafnTypedLinkName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.
--
-- /See:/ 'mkTypedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { schemaARN ::
      Lude.Text,
    typedLinkName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkSchemaAndFacetName' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
-- * 'typedLinkName' - The unique name of the typed link facet.
mkTypedLinkSchemaAndFacetName ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'typedLinkName'
  Lude.Text ->
  TypedLinkSchemaAndFacetName
mkTypedLinkSchemaAndFacetName pSchemaARN_ pTypedLinkName_ =
  TypedLinkSchemaAndFacetName'
    { schemaARN = pSchemaARN_,
      typedLinkName = pTypedLinkName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsafnSchemaARN :: Lens.Lens' TypedLinkSchemaAndFacetName Lude.Text
tlsafnSchemaARN = Lens.lens (schemaARN :: TypedLinkSchemaAndFacetName -> Lude.Text) (\s a -> s {schemaARN = a} :: TypedLinkSchemaAndFacetName)
{-# DEPRECATED tlsafnSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'typedLinkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsafnTypedLinkName :: Lens.Lens' TypedLinkSchemaAndFacetName Lude.Text
tlsafnTypedLinkName = Lens.lens (typedLinkName :: TypedLinkSchemaAndFacetName -> Lude.Text) (\s a -> s {typedLinkName = a} :: TypedLinkSchemaAndFacetName)
{-# DEPRECATED tlsafnTypedLinkName "Use generic-lens or generic-optics with 'typedLinkName' instead." #-}

instance Lude.FromJSON TypedLinkSchemaAndFacetName where
  parseJSON =
    Lude.withObject
      "TypedLinkSchemaAndFacetName"
      ( \x ->
          TypedLinkSchemaAndFacetName'
            Lude.<$> (x Lude..: "SchemaArn") Lude.<*> (x Lude..: "TypedLinkName")
      )

instance Lude.ToJSON TypedLinkSchemaAndFacetName where
  toJSON TypedLinkSchemaAndFacetName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaArn" Lude..= schemaARN),
            Lude.Just ("TypedLinkName" Lude..= typedLinkName)
          ]
      )
