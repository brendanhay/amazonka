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
-- Module      : Amazonka.DataBrew.Types.ThresholdType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ThresholdType
  ( ThresholdType
      ( ..,
        ThresholdType_GREATER_THAN,
        ThresholdType_GREATER_THAN_OR_EQUAL,
        ThresholdType_LESS_THAN,
        ThresholdType_LESS_THAN_OR_EQUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ThresholdType = ThresholdType'
  { fromThresholdType ::
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

pattern ThresholdType_GREATER_THAN :: ThresholdType
pattern ThresholdType_GREATER_THAN = ThresholdType' "GREATER_THAN"

pattern ThresholdType_GREATER_THAN_OR_EQUAL :: ThresholdType
pattern ThresholdType_GREATER_THAN_OR_EQUAL = ThresholdType' "GREATER_THAN_OR_EQUAL"

pattern ThresholdType_LESS_THAN :: ThresholdType
pattern ThresholdType_LESS_THAN = ThresholdType' "LESS_THAN"

pattern ThresholdType_LESS_THAN_OR_EQUAL :: ThresholdType
pattern ThresholdType_LESS_THAN_OR_EQUAL = ThresholdType' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  ThresholdType_GREATER_THAN,
  ThresholdType_GREATER_THAN_OR_EQUAL,
  ThresholdType_LESS_THAN,
  ThresholdType_LESS_THAN_OR_EQUAL,
  ThresholdType'
  #-}
