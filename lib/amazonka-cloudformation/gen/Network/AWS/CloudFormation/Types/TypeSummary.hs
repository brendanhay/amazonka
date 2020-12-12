{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeSummary
  ( TypeSummary (..),

    -- * Smart constructor
    mkTypeSummary,

    -- * Lenses
    tsLastUpdated,
    tsTypeName,
    tsDefaultVersionId,
    tsTypeARN,
    tsType,
    tsDescription,
  )
where

import Network.AWS.CloudFormation.Types.RegistryType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about the specified CloudFormation type.
--
-- /See:/ 'mkTypeSummary' smart constructor.
data TypeSummary = TypeSummary'
  { lastUpdated ::
      Lude.Maybe Lude.DateTime,
    typeName :: Lude.Maybe Lude.Text,
    defaultVersionId :: Lude.Maybe Lude.Text,
    typeARN :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe RegistryType,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypeSummary' with the minimum fields required to make a request.
--
-- * 'defaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
-- * 'description' - The description of the type.
-- * 'lastUpdated' - When the current default version of the type was registered.
-- * 'type'' - The kind of type.
-- * 'typeARN' - The Amazon Resource Name (ARN) of the type.
-- * 'typeName' - The name of the type.
mkTypeSummary ::
  TypeSummary
mkTypeSummary =
  TypeSummary'
    { lastUpdated = Lude.Nothing,
      typeName = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      typeARN = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | When the current default version of the type was registered.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastUpdated :: Lens.Lens' TypeSummary (Lude.Maybe Lude.DateTime)
tsLastUpdated = Lens.lens (lastUpdated :: TypeSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastUpdated = a} :: TypeSummary)
{-# DEPRECATED tsLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The name of the type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTypeName :: Lens.Lens' TypeSummary (Lude.Maybe Lude.Text)
tsTypeName = Lens.lens (typeName :: TypeSummary -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: TypeSummary)
{-# DEPRECATED tsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDefaultVersionId :: Lens.Lens' TypeSummary (Lude.Maybe Lude.Text)
tsDefaultVersionId = Lens.lens (defaultVersionId :: TypeSummary -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionId = a} :: TypeSummary)
{-# DEPRECATED tsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- /Note:/ Consider using 'typeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTypeARN :: Lens.Lens' TypeSummary (Lude.Maybe Lude.Text)
tsTypeARN = Lens.lens (typeARN :: TypeSummary -> Lude.Maybe Lude.Text) (\s a -> s {typeARN = a} :: TypeSummary)
{-# DEPRECATED tsTypeARN "Use generic-lens or generic-optics with 'typeARN' instead." #-}

-- | The kind of type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsType :: Lens.Lens' TypeSummary (Lude.Maybe RegistryType)
tsType = Lens.lens (type' :: TypeSummary -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: TypeSummary)
{-# DEPRECATED tsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDescription :: Lens.Lens' TypeSummary (Lude.Maybe Lude.Text)
tsDescription = Lens.lens (description :: TypeSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TypeSummary)
{-# DEPRECATED tsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML TypeSummary where
  parseXML x =
    TypeSummary'
      Lude.<$> (x Lude..@? "LastUpdated")
      Lude.<*> (x Lude..@? "TypeName")
      Lude.<*> (x Lude..@? "DefaultVersionId")
      Lude.<*> (x Lude..@? "TypeArn")
      Lude.<*> (x Lude..@? "Type")
      Lude.<*> (x Lude..@? "Description")
