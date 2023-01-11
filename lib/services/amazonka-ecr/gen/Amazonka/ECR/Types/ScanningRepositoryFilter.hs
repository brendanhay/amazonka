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
-- Module      : Amazonka.ECR.Types.ScanningRepositoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ScanningRepositoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ScanningRepositoryFilterType
import qualified Amazonka.Prelude as Prelude

-- | The details of a scanning repository filter. For more information on how
-- to use filters, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html#image-scanning-filters Using filters>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- /See:/ 'newScanningRepositoryFilter' smart constructor.
data ScanningRepositoryFilter = ScanningRepositoryFilter'
  { -- | The filter to use when scanning.
    filter' :: Prelude.Text,
    -- | The type associated with the filter.
    filterType :: ScanningRepositoryFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanningRepositoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'scanningRepositoryFilter_filter' - The filter to use when scanning.
--
-- 'filterType', 'scanningRepositoryFilter_filterType' - The type associated with the filter.
newScanningRepositoryFilter ::
  -- | 'filter''
  Prelude.Text ->
  -- | 'filterType'
  ScanningRepositoryFilterType ->
  ScanningRepositoryFilter
newScanningRepositoryFilter pFilter_ pFilterType_ =
  ScanningRepositoryFilter'
    { filter' = pFilter_,
      filterType = pFilterType_
    }

-- | The filter to use when scanning.
scanningRepositoryFilter_filter :: Lens.Lens' ScanningRepositoryFilter Prelude.Text
scanningRepositoryFilter_filter = Lens.lens (\ScanningRepositoryFilter' {filter'} -> filter') (\s@ScanningRepositoryFilter' {} a -> s {filter' = a} :: ScanningRepositoryFilter)

-- | The type associated with the filter.
scanningRepositoryFilter_filterType :: Lens.Lens' ScanningRepositoryFilter ScanningRepositoryFilterType
scanningRepositoryFilter_filterType = Lens.lens (\ScanningRepositoryFilter' {filterType} -> filterType) (\s@ScanningRepositoryFilter' {} a -> s {filterType = a} :: ScanningRepositoryFilter)

instance Data.FromJSON ScanningRepositoryFilter where
  parseJSON =
    Data.withObject
      "ScanningRepositoryFilter"
      ( \x ->
          ScanningRepositoryFilter'
            Prelude.<$> (x Data..: "filter")
            Prelude.<*> (x Data..: "filterType")
      )

instance Prelude.Hashable ScanningRepositoryFilter where
  hashWithSalt _salt ScanningRepositoryFilter' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` filterType

instance Prelude.NFData ScanningRepositoryFilter where
  rnf ScanningRepositoryFilter' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf filterType

instance Data.ToJSON ScanningRepositoryFilter where
  toJSON ScanningRepositoryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filter" Data..= filter'),
            Prelude.Just ("filterType" Data..= filterType)
          ]
      )
