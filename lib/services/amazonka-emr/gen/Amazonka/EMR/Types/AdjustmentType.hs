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
-- Module      : Amazonka.EMR.Types.AdjustmentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AdjustmentType
  ( AdjustmentType
      ( ..,
        AdjustmentType_CHANGE_IN_CAPACITY,
        AdjustmentType_EXACT_CAPACITY,
        AdjustmentType_PERCENT_CHANGE_IN_CAPACITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AdjustmentType = AdjustmentType'
  { fromAdjustmentType ::
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

pattern AdjustmentType_CHANGE_IN_CAPACITY :: AdjustmentType
pattern AdjustmentType_CHANGE_IN_CAPACITY = AdjustmentType' "CHANGE_IN_CAPACITY"

pattern AdjustmentType_EXACT_CAPACITY :: AdjustmentType
pattern AdjustmentType_EXACT_CAPACITY = AdjustmentType' "EXACT_CAPACITY"

pattern AdjustmentType_PERCENT_CHANGE_IN_CAPACITY :: AdjustmentType
pattern AdjustmentType_PERCENT_CHANGE_IN_CAPACITY = AdjustmentType' "PERCENT_CHANGE_IN_CAPACITY"

{-# COMPLETE
  AdjustmentType_CHANGE_IN_CAPACITY,
  AdjustmentType_EXACT_CAPACITY,
  AdjustmentType_PERCENT_CHANGE_IN_CAPACITY,
  AdjustmentType'
  #-}
