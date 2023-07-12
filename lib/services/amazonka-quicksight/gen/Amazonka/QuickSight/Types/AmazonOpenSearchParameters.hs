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
-- Module      : Amazonka.QuickSight.Types.AmazonOpenSearchParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AmazonOpenSearchParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for OpenSearch.
--
-- /See:/ 'newAmazonOpenSearchParameters' smart constructor.
data AmazonOpenSearchParameters = AmazonOpenSearchParameters'
  { -- | The OpenSearch domain.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'amazonOpenSearchParameters_domain' - The OpenSearch domain.
newAmazonOpenSearchParameters ::
  -- | 'domain'
  Prelude.Text ->
  AmazonOpenSearchParameters
newAmazonOpenSearchParameters pDomain_ =
  AmazonOpenSearchParameters' {domain = pDomain_}

-- | The OpenSearch domain.
amazonOpenSearchParameters_domain :: Lens.Lens' AmazonOpenSearchParameters Prelude.Text
amazonOpenSearchParameters_domain = Lens.lens (\AmazonOpenSearchParameters' {domain} -> domain) (\s@AmazonOpenSearchParameters' {} a -> s {domain = a} :: AmazonOpenSearchParameters)

instance Data.FromJSON AmazonOpenSearchParameters where
  parseJSON =
    Data.withObject
      "AmazonOpenSearchParameters"
      ( \x ->
          AmazonOpenSearchParameters'
            Prelude.<$> (x Data..: "Domain")
      )

instance Prelude.Hashable AmazonOpenSearchParameters where
  hashWithSalt _salt AmazonOpenSearchParameters' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData AmazonOpenSearchParameters where
  rnf AmazonOpenSearchParameters' {..} =
    Prelude.rnf domain

instance Data.ToJSON AmazonOpenSearchParameters where
  toJSON AmazonOpenSearchParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Domain" Data..= domain)]
      )
