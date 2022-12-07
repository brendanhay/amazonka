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
-- Module      : Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AwsIotAnalyticsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for IoT Analytics.
--
-- /See:/ 'newAwsIotAnalyticsParameters' smart constructor.
data AwsIotAnalyticsParameters = AwsIotAnalyticsParameters'
  { -- | Dataset name.
    dataSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIotAnalyticsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetName', 'awsIotAnalyticsParameters_dataSetName' - Dataset name.
newAwsIotAnalyticsParameters ::
  -- | 'dataSetName'
  Prelude.Text ->
  AwsIotAnalyticsParameters
newAwsIotAnalyticsParameters pDataSetName_ =
  AwsIotAnalyticsParameters'
    { dataSetName =
        pDataSetName_
    }

-- | Dataset name.
awsIotAnalyticsParameters_dataSetName :: Lens.Lens' AwsIotAnalyticsParameters Prelude.Text
awsIotAnalyticsParameters_dataSetName = Lens.lens (\AwsIotAnalyticsParameters' {dataSetName} -> dataSetName) (\s@AwsIotAnalyticsParameters' {} a -> s {dataSetName = a} :: AwsIotAnalyticsParameters)

instance Data.FromJSON AwsIotAnalyticsParameters where
  parseJSON =
    Data.withObject
      "AwsIotAnalyticsParameters"
      ( \x ->
          AwsIotAnalyticsParameters'
            Prelude.<$> (x Data..: "DataSetName")
      )

instance Prelude.Hashable AwsIotAnalyticsParameters where
  hashWithSalt _salt AwsIotAnalyticsParameters' {..} =
    _salt `Prelude.hashWithSalt` dataSetName

instance Prelude.NFData AwsIotAnalyticsParameters where
  rnf AwsIotAnalyticsParameters' {..} =
    Prelude.rnf dataSetName

instance Data.ToJSON AwsIotAnalyticsParameters where
  toJSON AwsIotAnalyticsParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DataSetName" Data..= dataSetName)]
      )
