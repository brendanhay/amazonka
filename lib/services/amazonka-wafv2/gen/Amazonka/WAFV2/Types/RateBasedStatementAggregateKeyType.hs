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
-- Module      : Amazonka.WAFV2.Types.RateBasedStatementAggregateKeyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateBasedStatementAggregateKeyType
  ( RateBasedStatementAggregateKeyType
      ( ..,
        RateBasedStatementAggregateKeyType_FORWARDED_IP,
        RateBasedStatementAggregateKeyType_IP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RateBasedStatementAggregateKeyType = RateBasedStatementAggregateKeyType'
  { fromRateBasedStatementAggregateKeyType ::
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

pattern RateBasedStatementAggregateKeyType_FORWARDED_IP :: RateBasedStatementAggregateKeyType
pattern RateBasedStatementAggregateKeyType_FORWARDED_IP = RateBasedStatementAggregateKeyType' "FORWARDED_IP"

pattern RateBasedStatementAggregateKeyType_IP :: RateBasedStatementAggregateKeyType
pattern RateBasedStatementAggregateKeyType_IP = RateBasedStatementAggregateKeyType' "IP"

{-# COMPLETE
  RateBasedStatementAggregateKeyType_FORWARDED_IP,
  RateBasedStatementAggregateKeyType_IP,
  RateBasedStatementAggregateKeyType'
  #-}
