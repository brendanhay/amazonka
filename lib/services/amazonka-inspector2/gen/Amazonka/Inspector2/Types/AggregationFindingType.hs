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
-- Module      : Amazonka.Inspector2.Types.AggregationFindingType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AggregationFindingType
  ( AggregationFindingType
      ( ..,
        AggregationFindingType_NETWORK_REACHABILITY,
        AggregationFindingType_PACKAGE_VULNERABILITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AggregationFindingType = AggregationFindingType'
  { fromAggregationFindingType ::
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

pattern AggregationFindingType_NETWORK_REACHABILITY :: AggregationFindingType
pattern AggregationFindingType_NETWORK_REACHABILITY = AggregationFindingType' "NETWORK_REACHABILITY"

pattern AggregationFindingType_PACKAGE_VULNERABILITY :: AggregationFindingType
pattern AggregationFindingType_PACKAGE_VULNERABILITY = AggregationFindingType' "PACKAGE_VULNERABILITY"

{-# COMPLETE
  AggregationFindingType_NETWORK_REACHABILITY,
  AggregationFindingType_PACKAGE_VULNERABILITY,
  AggregationFindingType'
  #-}
