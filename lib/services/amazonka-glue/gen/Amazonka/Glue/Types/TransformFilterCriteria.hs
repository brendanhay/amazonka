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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The time and date after which the transforms were created.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | The time and date before which the transforms were created.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | This value determines which version of Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | Filter on transforms last modified after this date.
    lastModifiedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter on transforms last modified before this date.
    lastModifiedBefore :: Prelude.Maybe Data.POSIX,
    -- | A unique transform name that is used to filter the machine learning
    -- transforms.
    name :: Prelude.Maybe Prelude.Text,
    -- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
    -- object is an array of key-value pairs representing the schema this
    -- transform accepts, where @Column@ is the name of a column, and @Type@ is
    -- the type of the data such as an integer or string. Has an upper bound of
    -- 100 columns.
    schema :: Prelude.Maybe [SchemaColumn],
    -- | Filters the list of machine learning transforms by the last known status
    -- of the transforms (to indicate whether a transform can be used or not).
    -- One of \"NOT_READY\", \"READY\", or \"DELETING\".
    status :: Prelude.Maybe TransformStatusType,
    -- | The type of machine learning transform that is used to filter the
    -- machine learning transforms.
    transformType :: Prelude.Maybe TransformType
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
-- 'createdAfter', 'transformFilterCriteria_createdAfter' - The time and date after which the transforms were created.
--
-- 'createdBefore', 'transformFilterCriteria_createdBefore' - The time and date before which the transforms were created.
--
-- 'glueVersion', 'transformFilterCriteria_glueVersion' - This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
--
-- 'lastModifiedAfter', 'transformFilterCriteria_lastModifiedAfter' - Filter on transforms last modified after this date.
--
-- 'lastModifiedBefore', 'transformFilterCriteria_lastModifiedBefore' - Filter on transforms last modified before this date.
--
-- 'name', 'transformFilterCriteria_name' - A unique transform name that is used to filter the machine learning
-- transforms.
--
-- 'schema', 'transformFilterCriteria_schema' - Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
--
-- 'status', 'transformFilterCriteria_status' - Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
--
-- 'transformType', 'transformFilterCriteria_transformType' - The type of machine learning transform that is used to filter the
-- machine learning transforms.
newTransformFilterCriteria ::
  TransformFilterCriteria
newTransformFilterCriteria =
  TransformFilterCriteria'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      lastModifiedAfter = Prelude.Nothing,
      lastModifiedBefore = Prelude.Nothing,
      name = Prelude.Nothing,
      schema = Prelude.Nothing,
      status = Prelude.Nothing,
      transformType = Prelude.Nothing
    }

-- | The time and date after which the transforms were created.
transformFilterCriteria_createdAfter :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_createdAfter = Lens.lens (\TransformFilterCriteria' {createdAfter} -> createdAfter) (\s@TransformFilterCriteria' {} a -> s {createdAfter = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | The time and date before which the transforms were created.
transformFilterCriteria_createdBefore :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_createdBefore = Lens.lens (\TransformFilterCriteria' {createdBefore} -> createdBefore) (\s@TransformFilterCriteria' {} a -> s {createdBefore = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
transformFilterCriteria_glueVersion :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.Text)
transformFilterCriteria_glueVersion = Lens.lens (\TransformFilterCriteria' {glueVersion} -> glueVersion) (\s@TransformFilterCriteria' {} a -> s {glueVersion = a} :: TransformFilterCriteria)

-- | Filter on transforms last modified after this date.
transformFilterCriteria_lastModifiedAfter :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_lastModifiedAfter = Lens.lens (\TransformFilterCriteria' {lastModifiedAfter} -> lastModifiedAfter) (\s@TransformFilterCriteria' {} a -> s {lastModifiedAfter = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter on transforms last modified before this date.
transformFilterCriteria_lastModifiedBefore :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.UTCTime)
transformFilterCriteria_lastModifiedBefore = Lens.lens (\TransformFilterCriteria' {lastModifiedBefore} -> lastModifiedBefore) (\s@TransformFilterCriteria' {} a -> s {lastModifiedBefore = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | A unique transform name that is used to filter the machine learning
-- transforms.
transformFilterCriteria_name :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe Prelude.Text)
transformFilterCriteria_name = Lens.lens (\TransformFilterCriteria' {name} -> name) (\s@TransformFilterCriteria' {} a -> s {name = a} :: TransformFilterCriteria)

-- | Filters on datasets with a specific schema. The @Map\<Column, Type>@
-- object is an array of key-value pairs representing the schema this
-- transform accepts, where @Column@ is the name of a column, and @Type@ is
-- the type of the data such as an integer or string. Has an upper bound of
-- 100 columns.
transformFilterCriteria_schema :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe [SchemaColumn])
transformFilterCriteria_schema = Lens.lens (\TransformFilterCriteria' {schema} -> schema) (\s@TransformFilterCriteria' {} a -> s {schema = a} :: TransformFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of machine learning transforms by the last known status
-- of the transforms (to indicate whether a transform can be used or not).
-- One of \"NOT_READY\", \"READY\", or \"DELETING\".
transformFilterCriteria_status :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe TransformStatusType)
transformFilterCriteria_status = Lens.lens (\TransformFilterCriteria' {status} -> status) (\s@TransformFilterCriteria' {} a -> s {status = a} :: TransformFilterCriteria)

-- | The type of machine learning transform that is used to filter the
-- machine learning transforms.
transformFilterCriteria_transformType :: Lens.Lens' TransformFilterCriteria (Prelude.Maybe TransformType)
transformFilterCriteria_transformType = Lens.lens (\TransformFilterCriteria' {transformType} -> transformType) (\s@TransformFilterCriteria' {} a -> s {transformType = a} :: TransformFilterCriteria)

instance Prelude.Hashable TransformFilterCriteria where
  hashWithSalt _salt TransformFilterCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` lastModifiedAfter
      `Prelude.hashWithSalt` lastModifiedBefore
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` transformType

instance Prelude.NFData TransformFilterCriteria where
  rnf TransformFilterCriteria' {..} =
    Prelude.rnf createdAfter `Prelude.seq`
      Prelude.rnf createdBefore `Prelude.seq`
        Prelude.rnf glueVersion `Prelude.seq`
          Prelude.rnf lastModifiedAfter `Prelude.seq`
            Prelude.rnf lastModifiedBefore `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf schema `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf transformType

instance Data.ToJSON TransformFilterCriteria where
  toJSON TransformFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("GlueVersion" Data..=) Prelude.<$> glueVersion,
            ("LastModifiedAfter" Data..=)
              Prelude.<$> lastModifiedAfter,
            ("LastModifiedBefore" Data..=)
              Prelude.<$> lastModifiedBefore,
            ("Name" Data..=) Prelude.<$> name,
            ("Schema" Data..=) Prelude.<$> schema,
            ("Status" Data..=) Prelude.<$> status,
            ("TransformType" Data..=) Prelude.<$> transformType
          ]
      )
