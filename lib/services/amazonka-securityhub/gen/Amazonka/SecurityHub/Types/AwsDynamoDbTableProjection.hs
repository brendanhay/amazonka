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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For global and local secondary indexes, identifies the attributes that
-- are copied from the table into the index.
--
-- /See:/ 'newAwsDynamoDbTableProjection' smart constructor.
data AwsDynamoDbTableProjection = AwsDynamoDbTableProjection'
  { -- | The nonkey attributes that are projected into the index. For each
    -- attribute, provide the attribute name.
    nonKeyAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The types of attributes that are projected into the index. Valid values
    -- are as follows:
    --
    -- -   @ALL@
    --
    -- -   @INCLUDE@
    --
    -- -   @KEYS_ONLY@
    projectionType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableProjection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonKeyAttributes', 'awsDynamoDbTableProjection_nonKeyAttributes' - The nonkey attributes that are projected into the index. For each
-- attribute, provide the attribute name.
--
-- 'projectionType', 'awsDynamoDbTableProjection_projectionType' - The types of attributes that are projected into the index. Valid values
-- are as follows:
--
-- -   @ALL@
--
-- -   @INCLUDE@
--
-- -   @KEYS_ONLY@
newAwsDynamoDbTableProjection ::
  AwsDynamoDbTableProjection
newAwsDynamoDbTableProjection =
  AwsDynamoDbTableProjection'
    { nonKeyAttributes =
        Prelude.Nothing,
      projectionType = Prelude.Nothing
    }

-- | The nonkey attributes that are projected into the index. For each
-- attribute, provide the attribute name.
awsDynamoDbTableProjection_nonKeyAttributes :: Lens.Lens' AwsDynamoDbTableProjection (Prelude.Maybe [Prelude.Text])
awsDynamoDbTableProjection_nonKeyAttributes = Lens.lens (\AwsDynamoDbTableProjection' {nonKeyAttributes} -> nonKeyAttributes) (\s@AwsDynamoDbTableProjection' {} a -> s {nonKeyAttributes = a} :: AwsDynamoDbTableProjection) Prelude.. Lens.mapping Lens.coerced

-- | The types of attributes that are projected into the index. Valid values
-- are as follows:
--
-- -   @ALL@
--
-- -   @INCLUDE@
--
-- -   @KEYS_ONLY@
awsDynamoDbTableProjection_projectionType :: Lens.Lens' AwsDynamoDbTableProjection (Prelude.Maybe Prelude.Text)
awsDynamoDbTableProjection_projectionType = Lens.lens (\AwsDynamoDbTableProjection' {projectionType} -> projectionType) (\s@AwsDynamoDbTableProjection' {} a -> s {projectionType = a} :: AwsDynamoDbTableProjection)

instance Data.FromJSON AwsDynamoDbTableProjection where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableProjection"
      ( \x ->
          AwsDynamoDbTableProjection'
            Prelude.<$> ( x
                            Data..:? "NonKeyAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProjectionType")
      )

instance Prelude.Hashable AwsDynamoDbTableProjection where
  hashWithSalt _salt AwsDynamoDbTableProjection' {..} =
    _salt
      `Prelude.hashWithSalt` nonKeyAttributes
      `Prelude.hashWithSalt` projectionType

instance Prelude.NFData AwsDynamoDbTableProjection where
  rnf AwsDynamoDbTableProjection' {..} =
    Prelude.rnf nonKeyAttributes
      `Prelude.seq` Prelude.rnf projectionType

instance Data.ToJSON AwsDynamoDbTableProjection where
  toJSON AwsDynamoDbTableProjection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NonKeyAttributes" Data..=)
              Prelude.<$> nonKeyAttributes,
            ("ProjectionType" Data..=)
              Prelude.<$> projectionType
          ]
      )
