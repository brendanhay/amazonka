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
-- Module      : Network.AWS.SageMaker.Types.CapacitySizeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CapacitySizeType
  ( CapacitySizeType
      ( ..,
        CapacitySizeType_CAPACITY_PERCENT,
        CapacitySizeType_INSTANCE_COUNT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CapacitySizeType = CapacitySizeType'
  { fromCapacitySizeType ::
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

pattern CapacitySizeType_CAPACITY_PERCENT :: CapacitySizeType
pattern CapacitySizeType_CAPACITY_PERCENT = CapacitySizeType' "CAPACITY_PERCENT"

pattern CapacitySizeType_INSTANCE_COUNT :: CapacitySizeType
pattern CapacitySizeType_INSTANCE_COUNT = CapacitySizeType' "INSTANCE_COUNT"

{-# COMPLETE
  CapacitySizeType_CAPACITY_PERCENT,
  CapacitySizeType_INSTANCE_COUNT,
  CapacitySizeType'
  #-}
