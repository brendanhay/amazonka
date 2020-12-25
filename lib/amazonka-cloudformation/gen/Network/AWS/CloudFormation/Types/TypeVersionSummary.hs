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
    tvsArn,
    tvsDescription,
    tvsIsDefaultVersion,
    tvsTimeCreated,
    tvsType,
    tvsTypeName,
    tvsVersionId,
  )
where

import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.RegistryType as Types
import qualified Network.AWS.CloudFormation.Types.TypeArn as Types
import qualified Network.AWS.CloudFormation.Types.TypeName as Types
import qualified Network.AWS.CloudFormation.Types.TypeVersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a specific version of a CloudFormation type.
--
-- /See:/ 'mkTypeVersionSummary' smart constructor.
data TypeVersionSummary = TypeVersionSummary'
  { -- | The Amazon Resource Name (ARN) of the type version.
    arn :: Core.Maybe Types.TypeArn,
    -- | The description of the type version.
    description :: Core.Maybe Types.Description,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | When the version was registered.
    timeCreated :: Core.Maybe Core.UTCTime,
    -- | The kind of type.
    type' :: Core.Maybe Types.RegistryType,
    -- | The name of the type.
    typeName :: Core.Maybe Types.TypeName,
    -- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
    versionId :: Core.Maybe Types.TypeVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TypeVersionSummary' value with any optional fields omitted.
mkTypeVersionSummary ::
  TypeVersionSummary
mkTypeVersionSummary =
  TypeVersionSummary'
    { arn = Core.Nothing,
      description = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      timeCreated = Core.Nothing,
      type' = Core.Nothing,
      typeName = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the type version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsArn :: Lens.Lens' TypeVersionSummary (Core.Maybe Types.TypeArn)
tvsArn = Lens.field @"arn"
{-# DEPRECATED tvsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the type version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsDescription :: Lens.Lens' TypeVersionSummary (Core.Maybe Types.Description)
tvsDescription = Lens.field @"description"
{-# DEPRECATED tvsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Whether the specified type version is set as the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsIsDefaultVersion :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.Bool)
tvsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED tvsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | When the version was registered.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsTimeCreated :: Lens.Lens' TypeVersionSummary (Core.Maybe Core.UTCTime)
tvsTimeCreated = Lens.field @"timeCreated"
{-# DEPRECATED tvsTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | The kind of type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsType :: Lens.Lens' TypeVersionSummary (Core.Maybe Types.RegistryType)
tvsType = Lens.field @"type'"
{-# DEPRECATED tvsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsTypeName :: Lens.Lens' TypeVersionSummary (Core.Maybe Types.TypeName)
tvsTypeName = Lens.field @"typeName"
{-# DEPRECATED tvsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvsVersionId :: Lens.Lens' TypeVersionSummary (Core.Maybe Types.TypeVersionId)
tvsVersionId = Lens.field @"versionId"
{-# DEPRECATED tvsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromXML TypeVersionSummary where
  parseXML x =
    TypeVersionSummary'
      Core.<$> (x Core..@? "Arn")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsDefaultVersion")
      Core.<*> (x Core..@? "TimeCreated")
      Core.<*> (x Core..@? "Type")
      Core.<*> (x Core..@? "TypeName")
      Core.<*> (x Core..@? "VersionId")
