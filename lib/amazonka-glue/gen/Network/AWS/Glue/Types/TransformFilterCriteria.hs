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
    tfcStatus,
    tfcLastModifiedAfter,
    tfcLastModifiedBefore,
    tfcGlueVersion,
    tfcSchema,
    tfcTransformType,
    tfcName,
    tfcCreatedBefore,
  )
where

import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria used to filter the machine learning transforms.
--
-- /See:/ 'mkTransformFilterCriteria' smart constructor.
data TransformFilterCriteria = TransformFilterCriteria'
  { -- | The time and date after which the transforms were created.
    createdAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
    status :: Lude.Maybe TransformStatusType,
    -- | Filter on transforms last modified after this date.
    lastModifiedAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filter on transforms last modified before this date.
    lastModifiedBefore :: Lude.Maybe Lude.Timestamp,
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
    schema :: Lude.Maybe [SchemaColumn],
    -- | The type of machine learning transform that is used to filter the machine learning transforms.
    transformType :: Lude.Maybe TransformType,
    -- | A unique transform name that is used to filter the machine learning transforms.
    name :: Lude.Maybe Lude.Text,
    -- | The time and date before which the transforms were created.
    createdBefore :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformFilterCriteria' with the minimum fields required to make a request.
--
-- * 'createdAfter' - The time and date after which the transforms were created.
-- * 'status' - Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
-- * 'lastModifiedAfter' - Filter on transforms last modified after this date.
-- * 'lastModifiedBefore' - Filter on transforms last modified before this date.
-- * 'glueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
-- * 'schema' - Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
-- * 'transformType' - The type of machine learning transform that is used to filter the machine learning transforms.
-- * 'name' - A unique transform name that is used to filter the machine learning transforms.
-- * 'createdBefore' - The time and date before which the transforms were created.
mkTransformFilterCriteria ::
  TransformFilterCriteria
mkTransformFilterCriteria =
  TransformFilterCriteria'
    { createdAfter = Lude.Nothing,
      status = Lude.Nothing,
      lastModifiedAfter = Lude.Nothing,
      lastModifiedBefore = Lude.Nothing,
      glueVersion = Lude.Nothing,
      schema = Lude.Nothing,
      transformType = Lude.Nothing,
      name = Lude.Nothing,
      createdBefore = Lude.Nothing
    }

-- | The time and date after which the transforms were created.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcCreatedAfter :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Timestamp)
tfcCreatedAfter = Lens.lens (createdAfter :: TransformFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | Filters the list of machine learning transforms by the last known status of the transforms (to indicate whether a transform can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcStatus :: Lens.Lens' TransformFilterCriteria (Lude.Maybe TransformStatusType)
tfcStatus = Lens.lens (status :: TransformFilterCriteria -> Lude.Maybe TransformStatusType) (\s a -> s {status = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filter on transforms last modified after this date.
--
-- /Note:/ Consider using 'lastModifiedAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcLastModifiedAfter :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Timestamp)
tfcLastModifiedAfter = Lens.lens (lastModifiedAfter :: TransformFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedAfter = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcLastModifiedAfter "Use generic-lens or generic-optics with 'lastModifiedAfter' instead." #-}

-- | Filter on transforms last modified before this date.
--
-- /Note:/ Consider using 'lastModifiedBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcLastModifiedBefore :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Timestamp)
tfcLastModifiedBefore = Lens.lens (lastModifiedBefore :: TransformFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedBefore = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcLastModifiedBefore "Use generic-lens or generic-optics with 'lastModifiedBefore' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcGlueVersion :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Text)
tfcGlueVersion = Lens.lens (glueVersion :: TransformFilterCriteria -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | Filters on datasets with a specific schema. The @Map<Column, Type>@ object is an array of key-value pairs representing the schema this transform accepts, where @Column@ is the name of a column, and @Type@ is the type of the data such as an integer or string. Has an upper bound of 100 columns.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcSchema :: Lens.Lens' TransformFilterCriteria (Lude.Maybe [SchemaColumn])
tfcSchema = Lens.lens (schema :: TransformFilterCriteria -> Lude.Maybe [SchemaColumn]) (\s a -> s {schema = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The type of machine learning transform that is used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'transformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcTransformType :: Lens.Lens' TransformFilterCriteria (Lude.Maybe TransformType)
tfcTransformType = Lens.lens (transformType :: TransformFilterCriteria -> Lude.Maybe TransformType) (\s a -> s {transformType = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcTransformType "Use generic-lens or generic-optics with 'transformType' instead." #-}

-- | A unique transform name that is used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcName :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Text)
tfcName = Lens.lens (name :: TransformFilterCriteria -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time and date before which the transforms were created.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfcCreatedBefore :: Lens.Lens' TransformFilterCriteria (Lude.Maybe Lude.Timestamp)
tfcCreatedBefore = Lens.lens (createdBefore :: TransformFilterCriteria -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: TransformFilterCriteria)
{-# DEPRECATED tfcCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

instance Lude.ToJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatedAfter" Lude..=) Lude.<$> createdAfter,
            ("Status" Lude..=) Lude.<$> status,
            ("LastModifiedAfter" Lude..=) Lude.<$> lastModifiedAfter,
            ("LastModifiedBefore" Lude..=) Lude.<$> lastModifiedBefore,
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            ("Schema" Lude..=) Lude.<$> schema,
            ("TransformType" Lude..=) Lude.<$> transformType,
            ("Name" Lude..=) Lude.<$> name,
            ("CreatedBefore" Lude..=) Lude.<$> createdBefore
          ]
      )
