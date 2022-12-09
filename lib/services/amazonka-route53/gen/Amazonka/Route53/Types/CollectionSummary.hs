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
-- Module      : Amazonka.Route53.Types.CollectionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CollectionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that is an entry in an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CidrCollection.html CidrCollection>
-- array.
--
-- /See:/ 'newCollectionSummary' smart constructor.
data CollectionSummary = CollectionSummary'
  { -- | The ARN of the collection summary. Can be used to reference the
    -- collection in IAM policy or cross-account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique ID for the CIDR collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of a CIDR collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | A sequential counter that Route 53 sets to 1 when you create a CIDR
    -- collection and increments by 1 each time you update settings for the
    -- CIDR collection.
    version :: Prelude.Maybe Prelude.Natural
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
-- 'arn', 'collectionSummary_arn' - The ARN of the collection summary. Can be used to reference the
-- collection in IAM policy or cross-account.
--
-- 'id', 'collectionSummary_id' - Unique ID for the CIDR collection.
--
-- 'name', 'collectionSummary_name' - The name of a CIDR collection.
--
-- 'version', 'collectionSummary_version' - A sequential counter that Route 53 sets to 1 when you create a CIDR
-- collection and increments by 1 each time you update settings for the
-- CIDR collection.
newCollectionSummary ::
  CollectionSummary
newCollectionSummary =
  CollectionSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ARN of the collection summary. Can be used to reference the
-- collection in IAM policy or cross-account.
collectionSummary_arn :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_arn = Lens.lens (\CollectionSummary' {arn} -> arn) (\s@CollectionSummary' {} a -> s {arn = a} :: CollectionSummary)

-- | Unique ID for the CIDR collection.
collectionSummary_id :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_id = Lens.lens (\CollectionSummary' {id} -> id) (\s@CollectionSummary' {} a -> s {id = a} :: CollectionSummary)

-- | The name of a CIDR collection.
collectionSummary_name :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Text)
collectionSummary_name = Lens.lens (\CollectionSummary' {name} -> name) (\s@CollectionSummary' {} a -> s {name = a} :: CollectionSummary)

-- | A sequential counter that Route 53 sets to 1 when you create a CIDR
-- collection and increments by 1 each time you update settings for the
-- CIDR collection.
collectionSummary_version :: Lens.Lens' CollectionSummary (Prelude.Maybe Prelude.Natural)
collectionSummary_version = Lens.lens (\CollectionSummary' {version} -> version) (\s@CollectionSummary' {} a -> s {version = a} :: CollectionSummary)

instance Data.FromXML CollectionSummary where
  parseXML x =
    CollectionSummary'
      Prelude.<$> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "Id")
      Prelude.<*> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Version")

instance Prelude.Hashable CollectionSummary where
  hashWithSalt _salt CollectionSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData CollectionSummary where
  rnf CollectionSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
