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
-- Module      : Network.AWS.S3.Types.AnalyticsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsFilter
import Network.AWS.S3.Types.StorageClassAnalysis

-- | Specifies the configuration and any analyses for the analytics filter of
-- an Amazon S3 bucket.
--
-- /See:/ 'newAnalyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { -- | The filter used to describe a set of objects for analyses. A filter must
    -- have exactly one prefix, one tag, or one conjunction
    -- (AnalyticsAndOperator). If no filter is provided, all objects will be
    -- considered in any analysis.
    filter' :: Core.Maybe AnalyticsFilter,
    -- | The ID that identifies the analytics configuration.
    id :: Core.Text,
    -- | Contains data related to access patterns to be collected and made
    -- available to analyze the tradeoffs between different storage classes.
    storageClassAnalysis :: StorageClassAnalysis
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalyticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'analyticsConfiguration_filter' - The filter used to describe a set of objects for analyses. A filter must
-- have exactly one prefix, one tag, or one conjunction
-- (AnalyticsAndOperator). If no filter is provided, all objects will be
-- considered in any analysis.
--
-- 'id', 'analyticsConfiguration_id' - The ID that identifies the analytics configuration.
--
-- 'storageClassAnalysis', 'analyticsConfiguration_storageClassAnalysis' - Contains data related to access patterns to be collected and made
-- available to analyze the tradeoffs between different storage classes.
newAnalyticsConfiguration ::
  -- | 'id'
  Core.Text ->
  -- | 'storageClassAnalysis'
  StorageClassAnalysis ->
  AnalyticsConfiguration
newAnalyticsConfiguration pId_ pStorageClassAnalysis_ =
  AnalyticsConfiguration'
    { filter' = Core.Nothing,
      id = pId_,
      storageClassAnalysis = pStorageClassAnalysis_
    }

-- | The filter used to describe a set of objects for analyses. A filter must
-- have exactly one prefix, one tag, or one conjunction
-- (AnalyticsAndOperator). If no filter is provided, all objects will be
-- considered in any analysis.
analyticsConfiguration_filter :: Lens.Lens' AnalyticsConfiguration (Core.Maybe AnalyticsFilter)
analyticsConfiguration_filter = Lens.lens (\AnalyticsConfiguration' {filter'} -> filter') (\s@AnalyticsConfiguration' {} a -> s {filter' = a} :: AnalyticsConfiguration)

-- | The ID that identifies the analytics configuration.
analyticsConfiguration_id :: Lens.Lens' AnalyticsConfiguration Core.Text
analyticsConfiguration_id = Lens.lens (\AnalyticsConfiguration' {id} -> id) (\s@AnalyticsConfiguration' {} a -> s {id = a} :: AnalyticsConfiguration)

-- | Contains data related to access patterns to be collected and made
-- available to analyze the tradeoffs between different storage classes.
analyticsConfiguration_storageClassAnalysis :: Lens.Lens' AnalyticsConfiguration StorageClassAnalysis
analyticsConfiguration_storageClassAnalysis = Lens.lens (\AnalyticsConfiguration' {storageClassAnalysis} -> storageClassAnalysis) (\s@AnalyticsConfiguration' {} a -> s {storageClassAnalysis = a} :: AnalyticsConfiguration)

instance Core.FromXML AnalyticsConfiguration where
  parseXML x =
    AnalyticsConfiguration'
      Core.<$> (x Core..@? "Filter")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "StorageClassAnalysis")

instance Core.Hashable AnalyticsConfiguration

instance Core.NFData AnalyticsConfiguration

instance Core.ToXML AnalyticsConfiguration where
  toXML AnalyticsConfiguration' {..} =
    Core.mconcat
      [ "Filter" Core.@= filter',
        "Id" Core.@= id,
        "StorageClassAnalysis" Core.@= storageClassAnalysis
      ]
