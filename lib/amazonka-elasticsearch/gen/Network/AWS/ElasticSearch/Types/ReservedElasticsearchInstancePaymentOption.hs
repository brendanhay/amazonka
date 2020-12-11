-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
  ( ReservedElasticsearchInstancePaymentOption
      ( ReservedElasticsearchInstancePaymentOption',
        AllUpfront,
        NoUpfront,
        PartialUpfront
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReservedElasticsearchInstancePaymentOption = ReservedElasticsearchInstancePaymentOption' Lude.Text
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

pattern AllUpfront :: ReservedElasticsearchInstancePaymentOption
pattern AllUpfront = ReservedElasticsearchInstancePaymentOption' "ALL_UPFRONT"

pattern NoUpfront :: ReservedElasticsearchInstancePaymentOption
pattern NoUpfront = ReservedElasticsearchInstancePaymentOption' "NO_UPFRONT"

pattern PartialUpfront :: ReservedElasticsearchInstancePaymentOption
pattern PartialUpfront = ReservedElasticsearchInstancePaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  AllUpfront,
  NoUpfront,
  PartialUpfront,
  ReservedElasticsearchInstancePaymentOption'
  #-}
