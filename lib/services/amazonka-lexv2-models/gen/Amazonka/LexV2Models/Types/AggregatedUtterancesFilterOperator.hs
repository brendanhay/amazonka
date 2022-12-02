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
-- Module      : Amazonka.LexV2Models.Types.AggregatedUtterancesFilterOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AggregatedUtterancesFilterOperator
  ( AggregatedUtterancesFilterOperator
      ( ..,
        AggregatedUtterancesFilterOperator_CO,
        AggregatedUtterancesFilterOperator_EQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregatedUtterancesFilterOperator = AggregatedUtterancesFilterOperator'
  { fromAggregatedUtterancesFilterOperator ::
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

pattern AggregatedUtterancesFilterOperator_CO :: AggregatedUtterancesFilterOperator
pattern AggregatedUtterancesFilterOperator_CO = AggregatedUtterancesFilterOperator' "CO"

pattern AggregatedUtterancesFilterOperator_EQ :: AggregatedUtterancesFilterOperator
pattern AggregatedUtterancesFilterOperator_EQ = AggregatedUtterancesFilterOperator' "EQ"

{-# COMPLETE
  AggregatedUtterancesFilterOperator_CO,
  AggregatedUtterancesFilterOperator_EQ,
  AggregatedUtterancesFilterOperator'
  #-}
