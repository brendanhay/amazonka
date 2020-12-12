{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DistributionMetricName
  ( DistributionMetricName
      ( DistributionMetricName',
        BytesDownloaded,
        BytesUploaded,
        HTTP4xxErrorRate,
        HTTP5xxErrorRate,
        Requests,
        TotalErrorRate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DistributionMetricName = DistributionMetricName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BytesDownloaded :: DistributionMetricName
pattern BytesDownloaded = DistributionMetricName' "BytesDownloaded"

pattern BytesUploaded :: DistributionMetricName
pattern BytesUploaded = DistributionMetricName' "BytesUploaded"

pattern HTTP4xxErrorRate :: DistributionMetricName
pattern HTTP4xxErrorRate = DistributionMetricName' "Http4xxErrorRate"

pattern HTTP5xxErrorRate :: DistributionMetricName
pattern HTTP5xxErrorRate = DistributionMetricName' "Http5xxErrorRate"

pattern Requests :: DistributionMetricName
pattern Requests = DistributionMetricName' "Requests"

pattern TotalErrorRate :: DistributionMetricName
pattern TotalErrorRate = DistributionMetricName' "TotalErrorRate"

{-# COMPLETE
  BytesDownloaded,
  BytesUploaded,
  HTTP4xxErrorRate,
  HTTP5xxErrorRate,
  Requests,
  TotalErrorRate,
  DistributionMetricName'
  #-}
