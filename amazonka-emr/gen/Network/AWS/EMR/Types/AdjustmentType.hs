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
-- Module      : Network.AWS.EMR.Types.AdjustmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AdjustmentType
  ( AdjustmentType
      ( ..,
        AdjustmentType_CHANGE_IN_CAPACITY,
        AdjustmentType_EXACT_CAPACITY,
        AdjustmentType_PERCENT_CHANGE_IN_CAPACITY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AdjustmentType = AdjustmentType'
  { fromAdjustmentType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
