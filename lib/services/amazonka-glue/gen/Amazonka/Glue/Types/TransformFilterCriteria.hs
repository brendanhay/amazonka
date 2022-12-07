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
-- Module      : Amazonka.Glue.Types.TransformFilterCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.SchemaColumn
import Amazonka.Glue.Types.TransformStatusType
import Amazonka.Glue.Types.TransformType
import qualified Amazonka.Prelude as Prelude

-- | The criteria used to filter the machine learning transforms.
--
-- /See:/ 'newTransformFilterCriteria' smart constructor.
data TransformFilterCriteria = TransformFilterCriteria'
  { -- | A unique transform name that is used to filter the machine learning
    -- transforms.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of machine learning transform that is used to filter the
    -- machine learning transforms.
    transformType :: Prelude.Maybe TransformType,
    -- | This value determines which version of Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The time and date before which the transforms were created.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of machine learning transforms by the last known status
    -- of the transforms (to indicate whether a transform can be used or not).
    -- One of \"NOT_READY\", \"READY\", or \"DELETING\".
    status :: Prelude.Maybe TransformStatusType,
    -- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
    -- object is an array of key-value pairs representing the schema this
    -- transform accepts, where @Column@ is the name of a column, and @Type@ is
    -- the type of the data such as an integer or string. Has an upper bound of
    -- 100 columns.
    schema :: Prelude.Maybe [SchemaColumn],
    -- | The time and date after which the transforms were created.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter on transforms last modified after this date.
    lastModifiedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter on transforms last modified before this date.
    lastModifiedBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'transformFilterCriteria_name' - A unique transform name that is used to filter the machine learning
-- transforms.
--
-- 'transformType', 'transformFilterCriteria_transformType' - The type of machine learning transform that is used to filter the
-- machine learning transforms.
--
-- 'glueVersion', 'transformFilterCriteria_glueVersion' - This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
--
-- 'createdBefore', 'transformFilterCriteria_createdBefore' - The time and date before which the transforms were created.
--
-- 'status', 'transformFilterCriteria_status' - Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
--
-- 'schema', 'transformFilterCriteria_schema' - Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
--
-- 'createdAfter', 'transformFilterCriteria_createdAfter' - The time and date after which the transforms were created.
--
-- 'lastModifiedAfter', 'transformFilterCriteria_lastModifiedAfter' - Filter on transforms last modified after this date.
--
-- 'lastModifiedBefore', 'transformFilterCriteria_lastModifiedBefore' - Filter on transforms last modified before this date.
newTransformFilterCriteria ::
  TransformFilterCriteria
newTransformFilterCriteria =
  TransformFilterCriteria'
    { name = Prelude.Nothing,
      transformType = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      status = Prelude.Nothing,
      schema = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      lastModifiedAfter = Prelude.Nothing,
      lastModifiedBefore = Prelude.Nothing
    }

-- | A unique transform name that is used to filter the machine learning
-- transforms.
transformFilterCriteria_name :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.Text)
transformFilterCriteria_name = Lens.lens (\TransformFilterCriteria' {name} -> name) (\s@TransformFilterCriteria' {} a -> s {name = a} :: TransformFilterCriteria)

-- | The type of machine learning transform that is used to filter the
-- machine learning transforms.
transformFilterCriteria_transformType :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe TransformType)
transformFilterCriteria_transformType = Lens.lens (\TransformFilterCriteria' {transformType} -> transformType) (\s@TransformFilterCriteria' {} a -> s {transformType = a} :: TransformFilterCriteria)

-- | This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
transformFilterCriteria_glueVersion :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.Text)
transformFilterCriteria_glueVersion = Lens.lens (\TransformFilterCriteria' {glueVersion} -> glueVersion) (\s@TransformFilterCriteria' {} a -> s {glueVersion = a} :: TransformFilterCriteria)

-- | The time and date before which the transforms were created.
transformFilterCriteria_createdBefore :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_createdBefore = Lens.lens (\TransformFilterCriteria' {createdBefore} -> createdBefore) (\s@TransformFilterCriteria' {} a -> s {createdBefore = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
transformFilterCriteria_status :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe TransformStatusType)
transformFilterCriteria_status = Lens.lens (\TransformFilterCriteria' {status} -> status) (\s@TransformFilterCriteria' {} a -> s {status = a} :: TransformFilterCriteria)

-- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
transformFilterCriteria_schema :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe [SchemaColumn])
transformFilterCriteria_schema = Lens.lens (\TransformFilterCriteria' {schema} -> schema) (\s@TransformFilterCriteria' {} a -> s {schema = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The time and date after which the transforms were created.
transformFilterCriteria_createdAfter :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_createdAfter = Lens.lens (\TransformFilterCriteria' {createdAfter} -> createdAfter) (\s@TransformFilterCriteria' {} a -> s {createdAfter = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter on transforms last modified after this date.
transformFilterCriteria_lastModifiedAfter :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_lastModifiedAfter = Lens.lens (\TransformFilterCriteria' {lastModifiedAfter} -> lastModifiedAfter) (\s@TransformFilterCriteria' {} a -> s {lastModifiedAfter = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter on transforms last modified before this date.
transformFilterCriteria_lastModifiedBefore :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_lastModifiedBefore = Lens.lens (\TransformFilterCriteria' {lastModifiedBefore} -> lastModifiedBefore) (\s@TransformFilterCriteria' {} a -> s {lastModifiedBefore = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable TransformFilterCriteria where
  hashWithSalt _salt TransformFilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` transformType
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` lastModifiedAfter
      `Prelude.hashWithSalt` lastModifiedBefore

instance Prelude.NFData TransformFilterCriteria where
  rnf TransformFilterCriteria' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf transformType
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf lastModifiedAfter
      `Prelude.seq` Prelude.rnf lastModifiedBefore

instance Data.ToJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("TransformType" Data..=) Prelude.<$> transformType,
            ("GlueVersion" Data..=) Prelude.<$> glueVersion,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("Status" Data..=) Prelude.<$> status,
            ("Schema" Data..=) Prelude.<$> schema,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("LastModifiedAfter" Data..=)
              Prelude.<$> lastModifiedAfter,
            ("LastModifiedBefore" Data..=)
              Prelude.<$> lastModifiedBefore
          ]
      )
