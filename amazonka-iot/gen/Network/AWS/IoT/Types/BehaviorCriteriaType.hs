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
-- Module      : Network.AWS.IoT.Types.BehaviorCriteriaType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BehaviorCriteriaType
  ( BehaviorCriteriaType
      ( ..,
        BehaviorCriteriaType_MACHINE_LEARNING,
        BehaviorCriteriaType_STATIC,
        BehaviorCriteriaType_STATISTICAL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BehaviorCriteriaType = BehaviorCriteriaType'
  { fromBehaviorCriteriaType ::
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

pattern BehaviorCriteriaType_MACHINE_LEARNING :: BehaviorCriteriaType
pattern BehaviorCriteriaType_MACHINE_LEARNING = BehaviorCriteriaType' "MACHINE_LEARNING"

pattern BehaviorCriteriaType_STATIC :: BehaviorCriteriaType
pattern BehaviorCriteriaType_STATIC = BehaviorCriteriaType' "STATIC"

pattern BehaviorCriteriaType_STATISTICAL :: BehaviorCriteriaType
pattern BehaviorCriteriaType_STATISTICAL = BehaviorCriteriaType' "STATISTICAL"

{-# COMPLETE
  BehaviorCriteriaType_MACHINE_LEARNING,
  BehaviorCriteriaType_STATIC,
  BehaviorCriteriaType_STATISTICAL,
  BehaviorCriteriaType'
  #-}
