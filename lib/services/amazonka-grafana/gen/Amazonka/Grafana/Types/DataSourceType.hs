{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Grafana.Types.DataSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_AMAZON_OPENSEARCH_SERVICE,
        DataSourceType_ATHENA,
        DataSourceType_CLOUDWATCH,
        DataSourceType_PROMETHEUS,
        DataSourceType_REDSHIFT,
        DataSourceType_SITEWISE,
        DataSourceType_TIMESTREAM,
        DataSourceType_TWINMAKER,
        DataSourceType_XRAY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern DataSourceType_AMAZON_OPENSEARCH_SERVICE :: DataSourceType
pattern DataSourceType_AMAZON_OPENSEARCH_SERVICE = DataSourceType' "AMAZON_OPENSEARCH_SERVICE"

pattern DataSourceType_ATHENA :: DataSourceType
pattern DataSourceType_ATHENA = DataSourceType' "ATHENA"

pattern DataSourceType_CLOUDWATCH :: DataSourceType
pattern DataSourceType_CLOUDWATCH = DataSourceType' "CLOUDWATCH"

pattern DataSourceType_PROMETHEUS :: DataSourceType
pattern DataSourceType_PROMETHEUS = DataSourceType' "PROMETHEUS"

pattern DataSourceType_REDSHIFT :: DataSourceType
pattern DataSourceType_REDSHIFT = DataSourceType' "REDSHIFT"

pattern DataSourceType_SITEWISE :: DataSourceType
pattern DataSourceType_SITEWISE = DataSourceType' "SITEWISE"

pattern DataSourceType_TIMESTREAM :: DataSourceType
pattern DataSourceType_TIMESTREAM = DataSourceType' "TIMESTREAM"

pattern DataSourceType_TWINMAKER :: DataSourceType
pattern DataSourceType_TWINMAKER = DataSourceType' "TWINMAKER"

pattern DataSourceType_XRAY :: DataSourceType
pattern DataSourceType_XRAY = DataSourceType' "XRAY"

{-# COMPLETE
  DataSourceType_AMAZON_OPENSEARCH_SERVICE,
  DataSourceType_ATHENA,
  DataSourceType_CLOUDWATCH,
  DataSourceType_PROMETHEUS,
  DataSourceType_REDSHIFT,
  DataSourceType_SITEWISE,
  DataSourceType_TIMESTREAM,
  DataSourceType_TWINMAKER,
  DataSourceType_XRAY,
  DataSourceType'
  #-}
