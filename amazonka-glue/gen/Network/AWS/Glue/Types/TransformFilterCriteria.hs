{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformFilterCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformFilterCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens

-- | The criteria used to filter the machine learning transforms.
--
-- /See:/ 'newTransformFilterCriteria' smart constructor.
data TransformFilterCriteria = TransformFilterCriteria'
  { -- | The time and date after which the transforms were created.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | Filters the list of machine learning transforms by the last known status
    -- of the transforms (to indicate whether a transform can be used or not).
    -- One of \"NOT_READY\", \"READY\", or \"DELETING\".
    status :: Core.Maybe TransformStatusType,
    -- | The type of machine learning transform that is used to filter the
    -- machine learning transforms.
    transformType :: Core.Maybe TransformType,
    -- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
    -- object is an array of key-value pairs representing the schema this
    -- transform accepts, where @Column@ is the name of a column, and @Type@ is
    -- the type of the data such as an integer or string. Has an upper bound of
    -- 100 columns.
    schema :: Core.Maybe [SchemaColumn],
    -- | The time and date before which the transforms were created.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | Filter on transforms last modified before this date.
    lastModifiedBefore :: Core.Maybe Core.POSIX,
    -- | Filter on transforms last modified after this date.
    lastModifiedAfter :: Core.Maybe Core.POSIX,
    -- | A unique transform name that is used to filter the machine learning
    -- transforms.
    name :: Core.Maybe Core.Text,
    -- | This value determines which version of AWS Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
    -- in the developer guide.
    glueVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransformFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'transformFilterCriteria_createdAfter' - The time and date after which the transforms were created.
--
-- 'status', 'transformFilterCriteria_status' - Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
--
-- 'transformType', 'transformFilterCriteria_transformType' - The type of machine learning transform that is used to filter the
-- machine learning transforms.
--
-- 'schema', 'transformFilterCriteria_schema' - Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
--
-- 'createdBefore', 'transformFilterCriteria_createdBefore' - The time and date before which the transforms were created.
--
-- 'lastModifiedBefore', 'transformFilterCriteria_lastModifiedBefore' - Filter on transforms last modified before this date.
--
-- 'lastModifiedAfter', 'transformFilterCriteria_lastModifiedAfter' - Filter on transforms last modified after this date.
--
-- 'name', 'transformFilterCriteria_name' - A unique transform name that is used to filter the machine learning
-- transforms.
--
-- 'glueVersion', 'transformFilterCriteria_glueVersion' - This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
newTransformFilterCriteria ::
  TransformFilterCriteria
newTransformFilterCriteria =
  TransformFilterCriteria'
    { createdAfter =
        Core.Nothing,
      status = Core.Nothing,
      transformType = Core.Nothing,
      schema = Core.Nothing,
      createdBefore = Core.Nothing,
      lastModifiedBefore = Core.Nothing,
      lastModifiedAfter = Core.Nothing,
      name = Core.Nothing,
      glueVersion = Core.Nothing
    }

-- | The time and date after which the transforms were created.
transformFilterCriteria_createdAfter :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.UTCTime)
transformFilterCriteria_createdAfter = Lens.lens (\TransformFilterCriteria' {createdAfter} -> createdAfter) (\s@TransformFilterCriteria' {} a -> s {createdAfter = a} :: TransformFilterCriteria) Core.. Lens.mapping Core._Time

-- | Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
transformFilterCriteria_status :: Lens.Lens' TransformFilterCriteria (Core.Maybe TransformStatusType)
transformFilterCriteria_status = Lens.lens (\TransformFilterCriteria' {status} -> status) (\s@TransformFilterCriteria' {} a -> s {status = a} :: TransformFilterCriteria)

-- | The type of machine learning transform that is used to filter the
-- machine learning transforms.
transformFilterCriteria_transformType :: Lens.Lens' TransformFilterCriteria (Core.Maybe TransformType)
transformFilterCriteria_transformType = Lens.lens (\TransformFilterCriteria' {transformType} -> transformType) (\s@TransformFilterCriteria' {} a -> s {transformType = a} :: TransformFilterCriteria)

-- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
transformFilterCriteria_schema :: Lens.Lens' TransformFilterCriteria (Core.Maybe [SchemaColumn])
transformFilterCriteria_schema = Lens.lens (\TransformFilterCriteria' {schema} -> schema) (\s@TransformFilterCriteria' {} a -> s {schema = a} :: TransformFilterCriteria) Core.. Lens.mapping Lens._Coerce

-- | The time and date before which the transforms were created.
transformFilterCriteria_createdBefore :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.UTCTime)
transformFilterCriteria_createdBefore = Lens.lens (\TransformFilterCriteria' {createdBefore} -> createdBefore) (\s@TransformFilterCriteria' {} a -> s {createdBefore = a} :: TransformFilterCriteria) Core.. Lens.mapping Core._Time

-- | Filter on transforms last modified before this date.
transformFilterCriteria_lastModifiedBefore :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.UTCTime)
transformFilterCriteria_lastModifiedBefore = Lens.lens (\TransformFilterCriteria' {lastModifiedBefore} -> lastModifiedBefore) (\s@TransformFilterCriteria' {} a -> s {lastModifiedBefore = a} :: TransformFilterCriteria) Core.. Lens.mapping Core._Time

-- | Filter on transforms last modified after this date.
transformFilterCriteria_lastModifiedAfter :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.UTCTime)
transformFilterCriteria_lastModifiedAfter = Lens.lens (\TransformFilterCriteria' {lastModifiedAfter} -> lastModifiedAfter) (\s@TransformFilterCriteria' {} a -> s {lastModifiedAfter = a} :: TransformFilterCriteria) Core.. Lens.mapping Core._Time

-- | A unique transform name that is used to filter the machine learning
-- transforms.
transformFilterCriteria_name :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.Text)
transformFilterCriteria_name = Lens.lens (\TransformFilterCriteria' {name} -> name) (\s@TransformFilterCriteria' {} a -> s {name = a} :: TransformFilterCriteria)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
transformFilterCriteria_glueVersion :: Lens.Lens' TransformFilterCriteria (Core.Maybe Core.Text)
transformFilterCriteria_glueVersion = Lens.lens (\TransformFilterCriteria' {glueVersion} -> glueVersion) (\s@TransformFilterCriteria' {} a -> s {glueVersion = a} :: TransformFilterCriteria)

instance Core.Hashable TransformFilterCriteria

instance Core.NFData TransformFilterCriteria

instance Core.ToJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("Status" Core..=) Core.<$> status,
            ("TransformType" Core..=) Core.<$> transformType,
            ("Schema" Core..=) Core.<$> schema,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("LastModifiedBefore" Core..=)
              Core.<$> lastModifiedBefore,
            ("LastModifiedAfter" Core..=)
              Core.<$> lastModifiedAfter,
            ("Name" Core..=) Core.<$> name,
            ("GlueVersion" Core..=) Core.<$> glueVersion
          ]
      )
