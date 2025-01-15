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
-- Module      : Amazonka.OpenSearchServerless.Types.UpdateCollectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.UpdateCollectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import Amazonka.OpenSearchServerless.Types.CollectionType
import qualified Amazonka.Prelude as Prelude

-- | Details about an updated OpenSearch Serverless collection.
--
-- /See:/ 'newUpdateCollectionDetail' smart constructor.
data UpdateCollectionDetail = UpdateCollectionDetail'
  { -- | The Amazon Resource Name (ARN) of the collection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the collection was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the collection was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the collection.
    status :: Prelude.Maybe CollectionStatus,
    -- | The collection type.
    type' :: Prelude.Maybe CollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCollectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateCollectionDetail_arn' - The Amazon Resource Name (ARN) of the collection.
--
-- 'createdDate', 'updateCollectionDetail_createdDate' - The date and time when the collection was created.
--
-- 'description', 'updateCollectionDetail_description' - The description of the collection.
--
-- 'id', 'updateCollectionDetail_id' - The unique identifier of the collection.
--
-- 'lastModifiedDate', 'updateCollectionDetail_lastModifiedDate' - The date and time when the collection was last modified.
--
-- 'name', 'updateCollectionDetail_name' - The name of the collection.
--
-- 'status', 'updateCollectionDetail_status' - The current status of the collection.
--
-- 'type'', 'updateCollectionDetail_type' - The collection type.
newUpdateCollectionDetail ::
  UpdateCollectionDetail
newUpdateCollectionDetail =
  UpdateCollectionDetail'
    { arn = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the collection.
updateCollectionDetail_arn :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Text)
updateCollectionDetail_arn = Lens.lens (\UpdateCollectionDetail' {arn} -> arn) (\s@UpdateCollectionDetail' {} a -> s {arn = a} :: UpdateCollectionDetail)

-- | The date and time when the collection was created.
updateCollectionDetail_createdDate :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Integer)
updateCollectionDetail_createdDate = Lens.lens (\UpdateCollectionDetail' {createdDate} -> createdDate) (\s@UpdateCollectionDetail' {} a -> s {createdDate = a} :: UpdateCollectionDetail)

-- | The description of the collection.
updateCollectionDetail_description :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Text)
updateCollectionDetail_description = Lens.lens (\UpdateCollectionDetail' {description} -> description) (\s@UpdateCollectionDetail' {} a -> s {description = a} :: UpdateCollectionDetail)

-- | The unique identifier of the collection.
updateCollectionDetail_id :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Text)
updateCollectionDetail_id = Lens.lens (\UpdateCollectionDetail' {id} -> id) (\s@UpdateCollectionDetail' {} a -> s {id = a} :: UpdateCollectionDetail)

-- | The date and time when the collection was last modified.
updateCollectionDetail_lastModifiedDate :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Integer)
updateCollectionDetail_lastModifiedDate = Lens.lens (\UpdateCollectionDetail' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateCollectionDetail' {} a -> s {lastModifiedDate = a} :: UpdateCollectionDetail)

-- | The name of the collection.
updateCollectionDetail_name :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe Prelude.Text)
updateCollectionDetail_name = Lens.lens (\UpdateCollectionDetail' {name} -> name) (\s@UpdateCollectionDetail' {} a -> s {name = a} :: UpdateCollectionDetail)

-- | The current status of the collection.
updateCollectionDetail_status :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe CollectionStatus)
updateCollectionDetail_status = Lens.lens (\UpdateCollectionDetail' {status} -> status) (\s@UpdateCollectionDetail' {} a -> s {status = a} :: UpdateCollectionDetail)

-- | The collection type.
updateCollectionDetail_type :: Lens.Lens' UpdateCollectionDetail (Prelude.Maybe CollectionType)
updateCollectionDetail_type = Lens.lens (\UpdateCollectionDetail' {type'} -> type') (\s@UpdateCollectionDetail' {} a -> s {type' = a} :: UpdateCollectionDetail)

instance Data.FromJSON UpdateCollectionDetail where
  parseJSON =
    Data.withObject
      "UpdateCollectionDetail"
      ( \x ->
          UpdateCollectionDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable UpdateCollectionDetail where
  hashWithSalt _salt UpdateCollectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UpdateCollectionDetail where
  rnf UpdateCollectionDetail' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdDate `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf lastModifiedDate `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf status `Prelude.seq`
                  Prelude.rnf type'
