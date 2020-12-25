{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformFilterCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformFilterCriteria
  ( TransformFilterCriteria (..),

    -- * Smart constructor
    mkTransformFilterCriteria,

    -- * Lenses
    tfcCreatedAfter,
    tfcCreatedBefore,
    tfcGlueVersion,
    tfcLastModifiedAfter,
    tfcLastModifiedBefore,
    tfcName,
    tfcSchema,
    tfcStatus,
    tfcTransformType,
  )
where

import qualified Network.AWS.Glue.Types.GlueVersionString as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.SchemaColumn as Types
import qualified Network.AWS.Glue.Types.TransformStatusType as Types
import qualified Network.AWS.Glue.Types.TransformType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria used to filter the machine learning transforms.
--
-- /See:/ 'mkTransformFilterCriteria' smart constructor.
data TransformFilterCriteria = TransformFilterCriteria'
  { -- | The time and date after which the transforms were created.
    createdAfter :: Core.Maybe Core.NominalDiffTime,
    -- | The time and date before which the transforms were created.
    createdBefore :: Core.Maybe Core.NominalDiffTime,
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Core.Maybe Types.GlueVersionString,
    -- | Filter on transforms last modified after this date.
    lastModifiedAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Filter on transforms last modified before this date.
    lastModifiedBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A unique transform name that is used to filter the machine learning transforms.
    name :: Core.Maybe Types.Name,
    -- | Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
    schema :: Core.Maybe [Types.SchemaColumn],
    -- | Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
    status :: Core.Maybe Types.TransformStatusType,
    -- | The type of machine learning transform that is used to filter the machine learning transforms.
    transformType :: Core.Maybe Types.TransformType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TransformFilterCriteria' value with any optional fields omitted.
mkTransformFilterCriteria ::
  TransformFilterCriteria
mkTransformFilterCriteria =
  TransformFilterCriteria'
    { createdAfter = Core.Nothing,
      createdBefore = Core.Nothing,
      glueVersion = Core.Nothing,
      lastModifiedAfter = Core.Nothing,
      lastModifiedBefore = Core.Nothing,
      name = Core.Nothing,
      schema = Core.Nothing,
      status = Core.Nothing,
      transformType = Core.Nothing
    }

-- | The time and date after which the transforms were created.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcCreatedAfter :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.NominalDiffTime)
tfcCreatedAfter = Lens.field @"createdAfter"
{-# DEPRECATED tfcCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | The time and date before which the transforms were created.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcCreatedBefore :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.NominalDiffTime)
tfcCreatedBefore = Lens.field @"createdBefore"
{-# DEPRECATED tfcCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcGlueVersion :: Lens.Lens' TransformFilterCriteria (Core.Maybe Types.GlueVersionString)
tfcGlueVersion = Lens.field @"glueVersion"
{-# DEPRECATED tfcGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | Filter on transforms last modified after this date.
--
-- /Note:/ Consider using 'lastModifiedAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcLastModifiedAfter :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.NominalDiffTime)
tfcLastModifiedAfter = Lens.field @"lastModifiedAfter"
{-# DEPRECATED tfcLastModifiedAfter "Use generic-lens or generic-optics with 'lastModifiedAfter' instead." #-}

-- | Filter on transforms last modified before this date.
--
-- /Note:/ Consider using 'lastModifiedBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcLastModifiedBefore :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.NominalDiffTime)
tfcLastModifiedBefore = Lens.field @"lastModifiedBefore"
{-# DEPRECATED tfcLastModifiedBefore "Use generic-lens or generic-optics with 'lastModifiedBefore' instead." #-}

-- | A unique transform name that is used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcName :: Lens.Lens' TransformFilterCriteria (Core.Maybe Types.Name)
tfcName = Lens.field @"name"
{-# DEPRECATED tfcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcSchema :: Lens.Lens' TransformFilterCriteria (Core.Maybe [Types.SchemaColumn])
tfcSchema = Lens.field @"schema"
{-# DEPRECATED tfcSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcStatus :: Lens.Lens' TransformFilterCriteria (Core.Maybe Types.TransformStatusType)
tfcStatus = Lens.field @"status"
{-# DEPRECATED tfcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of machine learning transform that is used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'transformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcTransformType :: Lens.Lens' TransformFilterCriteria (Core.Maybe Types.TransformType)
tfcTransformType = Lens.field @"transformType"
{-# DEPRECATED tfcTransformType "Use generic-lens or generic-optics with 'transformType' instead." #-}

instance Core.FromJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("LastModifiedAfter" Core..=) Core.<$> lastModifiedAfter,
            ("LastModifiedBefore" Core..=) Core.<$> lastModifiedBefore,
            ("Name" Core..=) Core.<$> name,
            ("Schema" Core..=) Core.<$> schema,
            ("Status" Core..=) Core.<$> status,
            ("TransformType" Core..=) Core.<$> transformType
          ]
      )
