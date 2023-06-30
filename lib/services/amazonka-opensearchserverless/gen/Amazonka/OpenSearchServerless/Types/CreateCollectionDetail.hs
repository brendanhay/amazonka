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
-- Module      : Amazonka.OpenSearchServerless.Types.CreateCollectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CreateCollectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import Amazonka.OpenSearchServerless.Types.CollectionType
import qualified Amazonka.Prelude as Prelude

-- | Details about the created OpenSearch Serverless collection.
--
-- /See:/ 'newCreateCollectionDetail' smart constructor.
data CreateCollectionDetail = CreateCollectionDetail'
  { -- | The Amazon Resource Name (ARN) of the collection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Epoch time when the collection was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | A description of the collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS key with which to encrypt the
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
-- Create a value of 'CreateCollectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createCollectionDetail_arn' - The Amazon Resource Name (ARN) of the collection.
--
-- 'createdDate', 'createCollectionDetail_createdDate' - The Epoch time when the collection was created.
--
-- 'description', 'createCollectionDetail_description' - A description of the collection.
--
-- 'id', 'createCollectionDetail_id' - The unique identifier of the collection.
--
-- 'kmsKeyArn', 'createCollectionDetail_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS key with which to encrypt the
-- collection.
--
-- 'lastModifiedDate', 'createCollectionDetail_lastModifiedDate' - The date and time when the collection was last modified.
--
-- 'name', 'createCollectionDetail_name' - The name of the collection.
--
-- 'status', 'createCollectionDetail_status' - The current status of the collection.
--
-- 'type'', 'createCollectionDetail_type' - The type of collection.
newCreateCollectionDetail ::
  CreateCollectionDetail
newCreateCollectionDetail =
  CreateCollectionDetail'
    { arn = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the collection.
createCollectionDetail_arn :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Text)
createCollectionDetail_arn = Lens.lens (\CreateCollectionDetail' {arn} -> arn) (\s@CreateCollectionDetail' {} a -> s {arn = a} :: CreateCollectionDetail)

-- | The Epoch time when the collection was created.
createCollectionDetail_createdDate :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Integer)
createCollectionDetail_createdDate = Lens.lens (\CreateCollectionDetail' {createdDate} -> createdDate) (\s@CreateCollectionDetail' {} a -> s {createdDate = a} :: CreateCollectionDetail)

-- | A description of the collection.
createCollectionDetail_description :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Text)
createCollectionDetail_description = Lens.lens (\CreateCollectionDetail' {description} -> description) (\s@CreateCollectionDetail' {} a -> s {description = a} :: CreateCollectionDetail)

-- | The unique identifier of the collection.
createCollectionDetail_id :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Text)
createCollectionDetail_id = Lens.lens (\CreateCollectionDetail' {id} -> id) (\s@CreateCollectionDetail' {} a -> s {id = a} :: CreateCollectionDetail)

-- | The Amazon Resource Name (ARN) of the KMS key with which to encrypt the
-- collection.
createCollectionDetail_kmsKeyArn :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Text)
createCollectionDetail_kmsKeyArn = Lens.lens (\CreateCollectionDetail' {kmsKeyArn} -> kmsKeyArn) (\s@CreateCollectionDetail' {} a -> s {kmsKeyArn = a} :: CreateCollectionDetail)

-- | The date and time when the collection was last modified.
createCollectionDetail_lastModifiedDate :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Integer)
createCollectionDetail_lastModifiedDate = Lens.lens (\CreateCollectionDetail' {lastModifiedDate} -> lastModifiedDate) (\s@CreateCollectionDetail' {} a -> s {lastModifiedDate = a} :: CreateCollectionDetail)

-- | The name of the collection.
createCollectionDetail_name :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe Prelude.Text)
createCollectionDetail_name = Lens.lens (\CreateCollectionDetail' {name} -> name) (\s@CreateCollectionDetail' {} a -> s {name = a} :: CreateCollectionDetail)

-- | The current status of the collection.
createCollectionDetail_status :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe CollectionStatus)
createCollectionDetail_status = Lens.lens (\CreateCollectionDetail' {status} -> status) (\s@CreateCollectionDetail' {} a -> s {status = a} :: CreateCollectionDetail)

-- | The type of collection.
createCollectionDetail_type :: Lens.Lens' CreateCollectionDetail (Prelude.Maybe CollectionType)
createCollectionDetail_type = Lens.lens (\CreateCollectionDetail' {type'} -> type') (\s@CreateCollectionDetail' {} a -> s {type' = a} :: CreateCollectionDetail)

instance Data.FromJSON CreateCollectionDetail where
  parseJSON =
    Data.withObject
      "CreateCollectionDetail"
      ( \x ->
          CreateCollectionDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable CreateCollectionDetail where
  hashWithSalt _salt CreateCollectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateCollectionDetail where
  rnf CreateCollectionDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
