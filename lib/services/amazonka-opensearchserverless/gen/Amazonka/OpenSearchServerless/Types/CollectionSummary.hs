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
-- Module      : Amazonka.OpenSearchServerless.Types.CollectionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CollectionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about each OpenSearch Serverless collection.
--
-- /See:/ 'newCollectionSummary' smart constructor.
data CollectionSummary = CollectionSummary'
  { -- | The Amazon Resource Name (ARN) of the collection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the collection.
    status :: Prelude.Maybe CollectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'collectionSummary_arn' - The Amazon Resource Name (ARN) of the collection.
--
-- 'id', 'collectionSummary_id' - The unique identifier of the collection.
--
-- 'name', 'collectionSummary_name' - The name of the collection.
--
-- 'status', 'collectionSummary_status' - The current status of the collection.
newCollectionSummary ::
  CollectionSummary
newCollectionSummary =
  CollectionSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the collection.
collectionSummary_arn :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_arn = Lens.lens (\CollectionSummary' {arn} -> arn) (\s@CollectionSummary' {} a -> s {arn = a} :: CollectionSummary)

-- | The unique identifier of the collection.
collectionSummary_id :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_id = Lens.lens (\CollectionSummary' {id} -> id) (\s@CollectionSummary' {} a -> s {id = a} :: CollectionSummary)

-- | The name of the collection.
collectionSummary_name :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_name = Lens.lens (\CollectionSummary' {name} -> name) (\s@CollectionSummary' {} a -> s {name = a} :: CollectionSummary)

-- | The current status of the collection.
collectionSummary_status :: Lens.Lens' CollectionSummary (Prelude.Maybe CollectionStatus)
collectionSummary_status = Lens.lens (\CollectionSummary' {status} -> status) (\s@CollectionSummary' {} a -> s {status = a} :: CollectionSummary)

instance Data.FromJSON CollectionSummary where
  parseJSON =
    Data.withObject
      "CollectionSummary"
      ( \x ->
          CollectionSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable CollectionSummary where
  hashWithSalt _salt CollectionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData CollectionSummary where
  rnf CollectionSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
