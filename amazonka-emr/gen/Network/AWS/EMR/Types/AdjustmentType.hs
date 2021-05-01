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

import qualified Network.AWS.Prelude as Prelude

newtype AdjustmentType = AdjustmentType'
  { fromAdjustmentType ::
      Prelude.Text
  }
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
