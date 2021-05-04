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
-- Module      : Network.AWS.Shield.Types.ProtectionGroupAggregation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupAggregation
  ( ProtectionGroupAggregation
      ( ..,
        ProtectionGroupAggregation_MAX,
        ProtectionGroupAggregation_MEAN,
        ProtectionGroupAggregation_SUM
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProtectionGroupAggregation = ProtectionGroupAggregation'
  { fromProtectionGroupAggregation ::
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
