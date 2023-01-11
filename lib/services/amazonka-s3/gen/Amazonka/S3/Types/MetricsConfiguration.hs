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
-- Module      : Amazonka.S3.Types.MetricsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.MetricsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.MetricsFilter

-- | Specifies a metrics configuration for the CloudWatch request metrics
-- (specified by the metrics configuration ID) from an Amazon S3 bucket. If
-- you\'re updating an existing metrics configuration, note that this is a
-- full replacement of the existing metrics configuration. If you don\'t
-- include the elements you want to keep, they are erased. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTMetricConfiguration.html PutBucketMetricsConfiguration>.
--
-- /See:/ 'newMetricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { -- | Specifies a metrics configuration filter. The metrics configuration will
    -- only include objects that meet the filter\'s criteria. A filter must be
    -- a prefix, an object tag, an access point ARN, or a conjunction
    -- (MetricsAndOperator).
    filter' :: Prelude.Maybe MetricsFilter,
    -- | The ID used to identify the metrics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- a prefix, an object tag, an access point ARN, or a conjunction
-- (MetricsAndOperator).
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
-- a prefix, an object tag, an access point ARN, or a conjunction
-- (MetricsAndOperator).
metricsConfiguration_filter :: Lens.Lens' MetricsConfiguration (Prelude.Maybe MetricsFilter)
metricsConfiguration_filter = Lens.lens (\MetricsConfiguration' {filter'} -> filter') (\s@MetricsConfiguration' {} a -> s {filter' = a} :: MetricsConfiguration)

-- | The ID used to identify the metrics configuration.
metricsConfiguration_id :: Lens.Lens' MetricsConfiguration Prelude.Text
metricsConfiguration_id = Lens.lens (\MetricsConfiguration' {id} -> id) (\s@MetricsConfiguration' {} a -> s {id = a} :: MetricsConfiguration)

instance Data.FromXML MetricsConfiguration where
  parseXML x =
    MetricsConfiguration'
      Prelude.<$> (x Data..@? "Filter") Prelude.<*> (x Data..@ "Id")

instance Prelude.Hashable MetricsConfiguration where
  hashWithSalt _salt MetricsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` id

instance Prelude.NFData MetricsConfiguration where
  rnf MetricsConfiguration' {..} =
    Prelude.rnf filter' `Prelude.seq` Prelude.rnf id

instance Data.ToXML MetricsConfiguration where
  toXML MetricsConfiguration' {..} =
    Prelude.mconcat
      ["Filter" Data.@= filter', "Id" Data.@= id]
