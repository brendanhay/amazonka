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
-- Module      : Amazonka.Lightsail.Types.DistributionMetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DistributionMetricName
  ( DistributionMetricName
      ( ..,
        DistributionMetricName_BytesDownloaded,
        DistributionMetricName_BytesUploaded,
        DistributionMetricName_Http4xxErrorRate,
        DistributionMetricName_Http5xxErrorRate,
        DistributionMetricName_Requests,
        DistributionMetricName_TotalErrorRate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DistributionMetricName = DistributionMetricName'
  { fromDistributionMetricName ::
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

pattern DistributionMetricName_BytesDownloaded :: DistributionMetricName
pattern DistributionMetricName_BytesDownloaded = DistributionMetricName' "BytesDownloaded"

pattern DistributionMetricName_BytesUploaded :: DistributionMetricName
pattern DistributionMetricName_BytesUploaded = DistributionMetricName' "BytesUploaded"

pattern DistributionMetricName_Http4xxErrorRate :: DistributionMetricName
pattern DistributionMetricName_Http4xxErrorRate = DistributionMetricName' "Http4xxErrorRate"

pattern DistributionMetricName_Http5xxErrorRate :: DistributionMetricName
pattern DistributionMetricName_Http5xxErrorRate = DistributionMetricName' "Http5xxErrorRate"

pattern DistributionMetricName_Requests :: DistributionMetricName
pattern DistributionMetricName_Requests = DistributionMetricName' "Requests"

pattern DistributionMetricName_TotalErrorRate :: DistributionMetricName
pattern DistributionMetricName_TotalErrorRate = DistributionMetricName' "TotalErrorRate"

{-# COMPLETE
  DistributionMetricName_BytesDownloaded,
  DistributionMetricName_BytesUploaded,
  DistributionMetricName_Http4xxErrorRate,
  DistributionMetricName_Http5xxErrorRate,
  DistributionMetricName_Requests,
  DistributionMetricName_TotalErrorRate,
  DistributionMetricName'
  #-}
