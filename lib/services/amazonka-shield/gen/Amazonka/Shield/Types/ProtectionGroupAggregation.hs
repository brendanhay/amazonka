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
-- Module      : Amazonka.Shield.Types.ProtectionGroupAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroupAggregation
  ( ProtectionGroupAggregation
      ( ..,
        ProtectionGroupAggregation_MAX,
        ProtectionGroupAggregation_MEAN,
        ProtectionGroupAggregation_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProtectionGroupAggregation = ProtectionGroupAggregation'
  { fromProtectionGroupAggregation ::
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

pattern ProtectionGroupAggregation_MAX :: ProtectionGroupAggregation
pattern ProtectionGroupAggregation_MAX = ProtectionGroupAggregation' "MAX"

pattern ProtectionGroupAggregation_MEAN :: ProtectionGroupAggregation
pattern ProtectionGroupAggregation_MEAN = ProtectionGroupAggregation' "MEAN"

pattern ProtectionGroupAggregation_SUM :: ProtectionGroupAggregation
pattern ProtectionGroupAggregation_SUM = ProtectionGroupAggregation' "SUM"

{-# COMPLETE
  ProtectionGroupAggregation_MAX,
  ProtectionGroupAggregation_MEAN,
  ProtectionGroupAggregation_SUM,
  ProtectionGroupAggregation'
  #-}
