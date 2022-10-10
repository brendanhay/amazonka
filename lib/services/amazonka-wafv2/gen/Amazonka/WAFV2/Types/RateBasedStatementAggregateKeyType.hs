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
import qualified Amazonka.Prelude as Prelude

newtype RateBasedStatementAggregateKeyType = RateBasedStatementAggregateKeyType'
  { fromRateBasedStatementAggregateKeyType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
