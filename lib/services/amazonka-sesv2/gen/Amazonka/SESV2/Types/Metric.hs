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
-- Module      : Amazonka.SESV2.Types.Metric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Metric
  ( Metric
      ( ..,
        Metric_CLICK,
        Metric_COMPLAINT,
        Metric_DELIVERY,
        Metric_DELIVERY_CLICK,
        Metric_DELIVERY_COMPLAINT,
        Metric_DELIVERY_OPEN,
        Metric_OPEN,
        Metric_PERMANENT_BOUNCE,
        Metric_SEND,
        Metric_TRANSIENT_BOUNCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Metric = Metric' {fromMetric :: Data.Text}
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

pattern Metric_CLICK :: Metric
pattern Metric_CLICK = Metric' "CLICK"

pattern Metric_COMPLAINT :: Metric
pattern Metric_COMPLAINT = Metric' "COMPLAINT"

pattern Metric_DELIVERY :: Metric
pattern Metric_DELIVERY = Metric' "DELIVERY"

pattern Metric_DELIVERY_CLICK :: Metric
pattern Metric_DELIVERY_CLICK = Metric' "DELIVERY_CLICK"

pattern Metric_DELIVERY_COMPLAINT :: Metric
pattern Metric_DELIVERY_COMPLAINT = Metric' "DELIVERY_COMPLAINT"

pattern Metric_DELIVERY_OPEN :: Metric
pattern Metric_DELIVERY_OPEN = Metric' "DELIVERY_OPEN"

pattern Metric_OPEN :: Metric
pattern Metric_OPEN = Metric' "OPEN"

pattern Metric_PERMANENT_BOUNCE :: Metric
pattern Metric_PERMANENT_BOUNCE = Metric' "PERMANENT_BOUNCE"

pattern Metric_SEND :: Metric
pattern Metric_SEND = Metric' "SEND"

pattern Metric_TRANSIENT_BOUNCE :: Metric
pattern Metric_TRANSIENT_BOUNCE = Metric' "TRANSIENT_BOUNCE"

{-# COMPLETE
  Metric_CLICK,
  Metric_COMPLAINT,
  Metric_DELIVERY,
  Metric_DELIVERY_CLICK,
  Metric_DELIVERY_COMPLAINT,
  Metric_DELIVERY_OPEN,
  Metric_OPEN,
  Metric_PERMANENT_BOUNCE,
  Metric_SEND,
  Metric_TRANSIENT_BOUNCE,
  Metric'
  #-}
