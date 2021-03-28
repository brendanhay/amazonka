{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DistributionMetricName
  ( DistributionMetricName
    ( DistributionMetricName'
    , DistributionMetricNameRequests
    , DistributionMetricNameBytesDownloaded
    , DistributionMetricNameBytesUploaded
    , DistributionMetricNameTotalErrorRate
    , DistributionMetricNameHttp4xxErrorRate
    , DistributionMetricNameHttp5xxErrorRate
    , fromDistributionMetricName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DistributionMetricName = DistributionMetricName'{fromDistributionMetricName
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern DistributionMetricNameRequests :: DistributionMetricName
pattern DistributionMetricNameRequests = DistributionMetricName' "Requests"

pattern DistributionMetricNameBytesDownloaded :: DistributionMetricName
pattern DistributionMetricNameBytesDownloaded = DistributionMetricName' "BytesDownloaded"

pattern DistributionMetricNameBytesUploaded :: DistributionMetricName
pattern DistributionMetricNameBytesUploaded = DistributionMetricName' "BytesUploaded"

pattern DistributionMetricNameTotalErrorRate :: DistributionMetricName
pattern DistributionMetricNameTotalErrorRate = DistributionMetricName' "TotalErrorRate"

pattern DistributionMetricNameHttp4xxErrorRate :: DistributionMetricName
pattern DistributionMetricNameHttp4xxErrorRate = DistributionMetricName' "Http4xxErrorRate"

pattern DistributionMetricNameHttp5xxErrorRate :: DistributionMetricName
pattern DistributionMetricNameHttp5xxErrorRate = DistributionMetricName' "Http5xxErrorRate"

{-# COMPLETE 
  DistributionMetricNameRequests,

  DistributionMetricNameBytesDownloaded,

  DistributionMetricNameBytesUploaded,

  DistributionMetricNameTotalErrorRate,

  DistributionMetricNameHttp4xxErrorRate,

  DistributionMetricNameHttp5xxErrorRate,
  DistributionMetricName'
  #-}
