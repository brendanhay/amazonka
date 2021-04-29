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
-- Module      : Network.AWS.CostExplorer.Types.Metric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Metric
  ( Metric
      ( ..,
        Metric_AMORTIZED_COST,
        Metric_BLENDED_COST,
        Metric_NET_AMORTIZED_COST,
        Metric_NET_UNBLENDED_COST,
        Metric_NORMALIZED_USAGE_AMOUNT,
        Metric_UNBLENDED_COST,
        Metric_USAGE_QUANTITY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype Metric = Metric' {fromMetric :: Prelude.Text}
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

pattern Metric_AMORTIZED_COST :: Metric
pattern Metric_AMORTIZED_COST = Metric' "AMORTIZED_COST"

pattern Metric_BLENDED_COST :: Metric
pattern Metric_BLENDED_COST = Metric' "BLENDED_COST"

pattern Metric_NET_AMORTIZED_COST :: Metric
pattern Metric_NET_AMORTIZED_COST = Metric' "NET_AMORTIZED_COST"

pattern Metric_NET_UNBLENDED_COST :: Metric
pattern Metric_NET_UNBLENDED_COST = Metric' "NET_UNBLENDED_COST"

pattern Metric_NORMALIZED_USAGE_AMOUNT :: Metric
pattern Metric_NORMALIZED_USAGE_AMOUNT = Metric' "NORMALIZED_USAGE_AMOUNT"

pattern Metric_UNBLENDED_COST :: Metric
pattern Metric_UNBLENDED_COST = Metric' "UNBLENDED_COST"

pattern Metric_USAGE_QUANTITY :: Metric
pattern Metric_USAGE_QUANTITY = Metric' "USAGE_QUANTITY"

{-# COMPLETE
  Metric_AMORTIZED_COST,
  Metric_BLENDED_COST,
  Metric_NET_AMORTIZED_COST,
  Metric_NET_UNBLENDED_COST,
  Metric_NORMALIZED_USAGE_AMOUNT,
  Metric_UNBLENDED_COST,
  Metric_USAGE_QUANTITY,
  Metric'
  #-}
