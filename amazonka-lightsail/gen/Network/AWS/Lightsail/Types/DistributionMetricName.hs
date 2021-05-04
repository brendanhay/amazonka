{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionMetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DistributionMetricName
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

import qualified Network.AWS.Prelude as Prelude

newtype DistributionMetricName = DistributionMetricName'
  { fromDistributionMetricName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
