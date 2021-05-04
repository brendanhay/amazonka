{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.MetricsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsFilter

-- | Specifies a metrics configuration for the CloudWatch request metrics
-- (specified by the metrics configuration ID) from an Amazon S3 bucket. If
-- you\'re updating an existing metrics configuration, note that this is a
-- full replacement of the existing metrics configuration. If you don\'t
-- include the elements you want to keep, they are erased. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTMetricConfiguration.html PUT Bucket metrics>
-- in the /Amazon Simple Storage Service API Reference/.
--
-- /See:/ 'newMetricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { -- | Specifies a metrics configuration filter. The metrics configuration will
    -- only include objects that meet the filter\'s criteria. A filter must be
    -- a prefix, a tag, or a conjunction (MetricsAndOperator).
    filter' :: Prelude.Maybe MetricsFilter,
    -- | The ID used to identify the metrics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'metricsConfiguration_filter' - Specifies a metrics configuration filter. The metrics configuration will
-- only include objects that meet the filter\'s criteria. A filter must be
-- a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- 'id', 'metricsConfiguration_id' - The ID used to identify the metrics configuration.
newMetricsConfiguration ::
  -- | 'id'
  Prelude.Text ->
  MetricsConfiguration
newMetricsConfiguration pId_ =
  MetricsConfiguration'
    { filter' = Prelude.Nothing,
      id = pId_
    }

-- | Specifies a metrics configuration filter. The metrics configuration will
-- only include objects that meet the filter\'s criteria. A filter must be
-- a prefix, a tag, or a conjunction (MetricsAndOperator).
metricsConfiguration_filter :: Lens.Lens' MetricsConfiguration (Prelude.Maybe MetricsFilter)
metricsConfiguration_filter = Lens.lens (\MetricsConfiguration' {filter'} -> filter') (\s@MetricsConfiguration' {} a -> s {filter' = a} :: MetricsConfiguration)

-- | The ID used to identify the metrics configuration.
metricsConfiguration_id :: Lens.Lens' MetricsConfiguration Prelude.Text
metricsConfiguration_id = Lens.lens (\MetricsConfiguration' {id} -> id) (\s@MetricsConfiguration' {} a -> s {id = a} :: MetricsConfiguration)

instance Prelude.FromXML MetricsConfiguration where
  parseXML x =
    MetricsConfiguration'
      Prelude.<$> (x Prelude..@? "Filter")
      Prelude.<*> (x Prelude..@ "Id")

instance Prelude.Hashable MetricsConfiguration

instance Prelude.NFData MetricsConfiguration

instance Prelude.ToXML MetricsConfiguration where
  toXML MetricsConfiguration' {..} =
    Prelude.mconcat
      ["Filter" Prelude.@= filter', "Id" Prelude.@= id]
