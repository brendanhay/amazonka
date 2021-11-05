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
-- Module      : Network.AWS.Forecast.Types.ScalingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.ScalingType
  ( ScalingType
      ( ..,
        ScalingType_Auto,
        ScalingType_Linear,
        ScalingType_Logarithmic,
        ScalingType_ReverseLogarithmic
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ScalingType = ScalingType'
  { fromScalingType ::
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

pattern ScalingType_Auto :: ScalingType
pattern ScalingType_Auto = ScalingType' "Auto"

pattern ScalingType_Linear :: ScalingType
pattern ScalingType_Linear = ScalingType' "Linear"

pattern ScalingType_Logarithmic :: ScalingType
pattern ScalingType_Logarithmic = ScalingType' "Logarithmic"

pattern ScalingType_ReverseLogarithmic :: ScalingType
pattern ScalingType_ReverseLogarithmic = ScalingType' "ReverseLogarithmic"

{-# COMPLETE
  ScalingType_Auto,
  ScalingType_Linear,
  ScalingType_Logarithmic,
  ScalingType_ReverseLogarithmic,
  ScalingType'
  #-}
