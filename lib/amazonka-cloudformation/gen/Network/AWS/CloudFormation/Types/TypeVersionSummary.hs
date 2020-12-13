{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TypeVersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeVersionSummary
  ( TypeVersionSummary (..),

    -- * Smart constructor
    mkTypeVersionSummary,

    -- * Lenses
    tvsVersionId,
    tvsTypeName,
    tvsARN,
    tvsTimeCreated,
    tvsType,
    tvsIsDefaultVersion,
    tvsDescription,
  )
where

import Network.AWS.CloudFormation.Types.RegistryType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a specific version of a CloudFormation type.
--
-- /See:/ 'mkTypeVersionSummary' smart constructor.
data TypeVersionSummary = TypeVersionSummary'
  { -- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
    versionId :: Lude.Maybe Lude.Text,
    -- | The name of the type.
    typeName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the type version.
    arn :: Lude.Maybe Lude.Text,
    -- | When the version was registered.
    timeCreated :: Lude.Maybe Lude.DateTime,
    -- | The kind of type.
    type' :: Lude.Maybe RegistryType,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Lude.Maybe Lude.Bool,
    -- | The description of the type version.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypeVersionSummary' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
-- * 'typeName' - The name of the type.
-- * 'arn' - The Amazon Resource Name (ARN) of the type version.
-- * 'timeCreated' - When the version was registered.
-- * 'type'' - The kind of type.
-- * 'isDefaultVersion' - Whether the specified type version is set as the default version.
-- * 'description' - The description of the type version.
mkTypeVersionSummary ::
  TypeVersionSummary
mkTypeVersionSummary =
  TypeVersionSummary'
    { versionId = Lude.Nothing,
      typeName = Lude.Nothing,
      arn = Lude.Nothing,
      timeCreated = Lude.Nothing,
      type' = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsVersionId :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.Text)
tvsVersionId = Lens.lens (versionId :: TypeVersionSummary -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: TypeVersionSummary)
{-# DEPRECATED tvsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsTypeName :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.Text)
tvsTypeName = Lens.lens (typeName :: TypeVersionSummary -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: TypeVersionSummary)
{-# DEPRECATED tvsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsARN :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.Text)
tvsARN = Lens.lens (arn :: TypeVersionSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TypeVersionSummary)
{-# DEPRECATED tvsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the version was registered.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsTimeCreated :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.DateTime)
tvsTimeCreated = Lens.lens (timeCreated :: TypeVersionSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {timeCreated = a} :: TypeVersionSummary)
{-# DEPRECATED tvsTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | The kind of type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsType :: Lens.Lens' TypeVersionSummary (Lude.Maybe RegistryType)
tvsType = Lens.lens (type' :: TypeVersionSummary -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: TypeVersionSummary)
{-# DEPRECATED tvsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Whether the specified type version is set as the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsIsDefaultVersion :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.Bool)
tvsIsDefaultVersion = Lens.lens (isDefaultVersion :: TypeVersionSummary -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: TypeVersionSummary)
{-# DEPRECATED tvsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The description of the type version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsDescription :: Lens.Lens' TypeVersionSummary (Lude.Maybe Lude.Text)
tvsDescription = Lens.lens (description :: TypeVersionSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TypeVersionSummary)
{-# DEPRECATED tvsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML TypeVersionSummary where
  parseXML x =
    TypeVersionSummary'
      Lude.<$> (x Lude..@? "VersionId")
      Lude.<*> (x Lude..@? "TypeName")
      Lude.<*> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "TimeCreated")
      Lude.<*> (x Lude..@? "Type")
      Lude.<*> (x Lude..@? "IsDefaultVersion")
      Lude.<*> (x Lude..@? "Description")
