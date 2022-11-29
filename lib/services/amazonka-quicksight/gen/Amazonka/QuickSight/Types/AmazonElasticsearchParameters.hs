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
-- Module      : Amazonka.QuickSight.Types.AmazonElasticsearchParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AmazonElasticsearchParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The parameters for OpenSearch.
--
-- /See:/ 'newAmazonElasticsearchParameters' smart constructor.
data AmazonElasticsearchParameters = AmazonElasticsearchParameters'
  { -- | The OpenSearch domain.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonElasticsearchParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'amazonElasticsearchParameters_domain' - The OpenSearch domain.
newAmazonElasticsearchParameters ::
  -- | 'domain'
  Prelude.Text ->
  AmazonElasticsearchParameters
newAmazonElasticsearchParameters pDomain_ =
  AmazonElasticsearchParameters' {domain = pDomain_}

-- | The OpenSearch domain.
amazonElasticsearchParameters_domain :: Lens.Lens' AmazonElasticsearchParameters Prelude.Text
amazonElasticsearchParameters_domain = Lens.lens (\AmazonElasticsearchParameters' {domain} -> domain) (\s@AmazonElasticsearchParameters' {} a -> s {domain = a} :: AmazonElasticsearchParameters)

instance Core.FromJSON AmazonElasticsearchParameters where
  parseJSON =
    Core.withObject
      "AmazonElasticsearchParameters"
      ( \x ->
          AmazonElasticsearchParameters'
            Prelude.<$> (x Core..: "Domain")
      )

instance
  Prelude.Hashable
    AmazonElasticsearchParameters
  where
  hashWithSalt _salt AmazonElasticsearchParameters' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData AmazonElasticsearchParameters where
  rnf AmazonElasticsearchParameters' {..} =
    Prelude.rnf domain

instance Core.ToJSON AmazonElasticsearchParameters where
  toJSON AmazonElasticsearchParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Domain" Core..= domain)]
      )
