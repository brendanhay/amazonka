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
-- Module      : Amazonka.OpenSearchServerless.Types.CollectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CollectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import Amazonka.OpenSearchServerless.Types.CollectionType
import qualified Amazonka.Prelude as Prelude

-- | Details about each OpenSearch Serverless collection, including the
-- collection endpoint and the OpenSearch Dashboards endpoint.
--
-- /See:/ 'newCollectionDetail' smart constructor.
data CollectionDetail = CollectionDetail'
  { -- | The Amazon Resource Name (ARN) of the collection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Collection-specific endpoint used to submit index, search, and data
    -- upload requests to an OpenSearch Serverless collection.
    collectionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Epoch time when the collection was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | Collection-specific endpoint used to access OpenSearch Dashboards.
    dashboardEndpoint :: Prelude.Maybe Prelude.Text,
    -- | A description of the collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Web Services KMS key used to encrypt the
    -- collection.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the collection was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the collection.
    status :: Prelude.Maybe CollectionStatus,
    -- | The type of collection.
    type' :: Prelude.Maybe CollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'collectionDetail_arn' - The Amazon Resource Name (ARN) of the collection.
--
-- 'collectionEndpoint', 'collectionDetail_collectionEndpoint' - Collection-specific endpoint used to submit index, search, and data
-- upload requests to an OpenSearch Serverless collection.
--
-- 'createdDate', 'collectionDetail_createdDate' - The Epoch time when the collection was created.
--
-- 'dashboardEndpoint', 'collectionDetail_dashboardEndpoint' - Collection-specific endpoint used to access OpenSearch Dashboards.
--
-- 'description', 'collectionDetail_description' - A description of the collection.
--
-- 'id', 'collectionDetail_id' - A unique identifier for the collection.
--
-- 'kmsKeyArn', 'collectionDetail_kmsKeyArn' - The ARN of the Amazon Web Services KMS key used to encrypt the
-- collection.
--
-- 'lastModifiedDate', 'collectionDetail_lastModifiedDate' - The date and time when the collection was last modified.
--
-- 'name', 'collectionDetail_name' - The name of the collection.
--
-- 'status', 'collectionDetail_status' - The current status of the collection.
--
-- 'type'', 'collectionDetail_type' - The type of collection.
newCollectionDetail ::
  CollectionDetail
newCollectionDetail =
  CollectionDetail'
    { arn = Prelude.Nothing,
      collectionEndpoint = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      dashboardEndpoint = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the collection.
collectionDetail_arn :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_arn = Lens.lens (\CollectionDetail' {arn} -> arn) (\s@CollectionDetail' {} a -> s {arn = a} :: CollectionDetail)

-- | Collection-specific endpoint used to submit index, search, and data
-- upload requests to an OpenSearch Serverless collection.
collectionDetail_collectionEndpoint :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_collectionEndpoint = Lens.lens (\CollectionDetail' {collectionEndpoint} -> collectionEndpoint) (\s@CollectionDetail' {} a -> s {collectionEndpoint = a} :: CollectionDetail)

-- | The Epoch time when the collection was created.
collectionDetail_createdDate :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Integer)
collectionDetail_createdDate = Lens.lens (\CollectionDetail' {createdDate} -> createdDate) (\s@CollectionDetail' {} a -> s {createdDate = a} :: CollectionDetail)

-- | Collection-specific endpoint used to access OpenSearch Dashboards.
collectionDetail_dashboardEndpoint :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_dashboardEndpoint = Lens.lens (\CollectionDetail' {dashboardEndpoint} -> dashboardEndpoint) (\s@CollectionDetail' {} a -> s {dashboardEndpoint = a} :: CollectionDetail)

-- | A description of the collection.
collectionDetail_description :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_description = Lens.lens (\CollectionDetail' {description} -> description) (\s@CollectionDetail' {} a -> s {description = a} :: CollectionDetail)

-- | A unique identifier for the collection.
collectionDetail_id :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_id = Lens.lens (\CollectionDetail' {id} -> id) (\s@CollectionDetail' {} a -> s {id = a} :: CollectionDetail)

-- | The ARN of the Amazon Web Services KMS key used to encrypt the
-- collection.
collectionDetail_kmsKeyArn :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_kmsKeyArn = Lens.lens (\CollectionDetail' {kmsKeyArn} -> kmsKeyArn) (\s@CollectionDetail' {} a -> s {kmsKeyArn = a} :: CollectionDetail)

-- | The date and time when the collection was last modified.
collectionDetail_lastModifiedDate :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Integer)
collectionDetail_lastModifiedDate = Lens.lens (\CollectionDetail' {lastModifiedDate} -> lastModifiedDate) (\s@CollectionDetail' {} a -> s {lastModifiedDate = a} :: CollectionDetail)

-- | The name of the collection.
collectionDetail_name :: Lens.Lens' CollectionDetail (Prelude.Maybe Prelude.Text)
collectionDetail_name = Lens.lens (\CollectionDetail' {name} -> name) (\s@CollectionDetail' {} a -> s {name = a} :: CollectionDetail)

-- | The current status of the collection.
collectionDetail_status :: Lens.Lens' CollectionDetail (Prelude.Maybe CollectionStatus)
collectionDetail_status = Lens.lens (\CollectionDetail' {status} -> status) (\s@CollectionDetail' {} a -> s {status = a} :: CollectionDetail)

-- | The type of collection.
collectionDetail_type :: Lens.Lens' CollectionDetail (Prelude.Maybe CollectionType)
collectionDetail_type = Lens.lens (\CollectionDetail' {type'} -> type') (\s@CollectionDetail' {} a -> s {type' = a} :: CollectionDetail)

instance Data.FromJSON CollectionDetail where
  parseJSON =
    Data.withObject
      "CollectionDetail"
      ( \x ->
          CollectionDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "collectionEndpoint")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "dashboardEndpoint")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable CollectionDetail where
  hashWithSalt _salt CollectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` collectionEndpoint
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` dashboardEndpoint
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CollectionDetail where
  rnf CollectionDetail' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf collectionEndpoint `Prelude.seq`
        Prelude.rnf createdDate `Prelude.seq`
          Prelude.rnf dashboardEndpoint `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf id `Prelude.seq`
                Prelude.rnf kmsKeyArn `Prelude.seq`
                  Prelude.rnf lastModifiedDate `Prelude.seq`
                    Prelude.rnf name `Prelude.seq`
                      Prelude.rnf status `Prelude.seq`
                        Prelude.rnf type'
