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
-- Module      : Amazonka.ResourceExplorer2.Types.Index
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.Index where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceExplorer2.Types.IndexType

-- | An index is the data store used by Amazon Web Services Resource Explorer
-- to hold information about your Amazon Web Services resources that the
-- service discovers. Creating an index in an Amazon Web Services Region
-- turns on Resource Explorer and lets it discover your resources.
--
-- By default, an index is /local/, meaning that it contains information
-- about resources in only the same Region as the index. However, you can
-- promote the index of one Region in the account by calling
-- UpdateIndexType to convert it into an aggregator index. The aggregator
-- index receives a replicated copy of the index information from all other
-- Regions where Resource Explorer is turned on. This allows search
-- operations in that Region to return results from all Regions in the
-- account.
--
-- /See:/ 'newIndex' smart constructor.
data Index = Index'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the index.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region in which the index exists.
    region :: Prelude.Maybe Prelude.Text,
    -- | The type of index. It can be one of the following values:
    --
    -- -   __LOCAL__ – The index contains information about resources from only
    --     the same Amazon Web Services Region.
    --
    -- -   __AGGREGATOR__ – Resource Explorer replicates copies of the indexed
    --     information about resources in all other Amazon Web Services Regions
    --     to the aggregator index. This lets search results in the Region with
    --     the aggregator index to include resources from all Regions in the
    --     account where Resource Explorer is turned on.
    type' :: Prelude.Maybe IndexType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Index' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'index_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index.
--
-- 'region', 'index_region' - The Amazon Web Services Region in which the index exists.
--
-- 'type'', 'index_type' - The type of index. It can be one of the following values:
--
-- -   __LOCAL__ – The index contains information about resources from only
--     the same Amazon Web Services Region.
--
-- -   __AGGREGATOR__ – Resource Explorer replicates copies of the indexed
--     information about resources in all other Amazon Web Services Regions
--     to the aggregator index. This lets search results in the Region with
--     the aggregator index to include resources from all Regions in the
--     account where Resource Explorer is turned on.
newIndex ::
  Index
newIndex =
  Index'
    { arn = Prelude.Nothing,
      region = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index.
index_arn :: Lens.Lens' Index (Prelude.Maybe Prelude.Text)
index_arn = Lens.lens (\Index' {arn} -> arn) (\s@Index' {} a -> s {arn = a} :: Index)

-- | The Amazon Web Services Region in which the index exists.
index_region :: Lens.Lens' Index (Prelude.Maybe Prelude.Text)
index_region = Lens.lens (\Index' {region} -> region) (\s@Index' {} a -> s {region = a} :: Index)

-- | The type of index. It can be one of the following values:
--
-- -   __LOCAL__ – The index contains information about resources from only
--     the same Amazon Web Services Region.
--
-- -   __AGGREGATOR__ – Resource Explorer replicates copies of the indexed
--     information about resources in all other Amazon Web Services Regions
--     to the aggregator index. This lets search results in the Region with
--     the aggregator index to include resources from all Regions in the
--     account where Resource Explorer is turned on.
index_type :: Lens.Lens' Index (Prelude.Maybe IndexType)
index_type = Lens.lens (\Index' {type'} -> type') (\s@Index' {} a -> s {type' = a} :: Index)

instance Data.FromJSON Index where
  parseJSON =
    Data.withObject
      "Index"
      ( \x ->
          Index'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Index where
  hashWithSalt _salt Index' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Index where
  rnf Index' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf type'
