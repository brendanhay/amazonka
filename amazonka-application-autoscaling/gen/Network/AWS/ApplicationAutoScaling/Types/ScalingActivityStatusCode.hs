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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
  ( ScalingActivityStatusCode
      ( ..,
        ScalingActivityStatusCode_Failed,
        ScalingActivityStatusCode_InProgress,
        ScalingActivityStatusCode_Overridden,
        ScalingActivityStatusCode_Pending,
        ScalingActivityStatusCode_Successful,
        ScalingActivityStatusCode_Unfulfilled
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ScalingActivityStatusCode = ScalingActivityStatusCode'
  { fromScalingActivityStatusCode ::
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

pattern ScalingActivityStatusCode_Failed :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Failed = ScalingActivityStatusCode' "Failed"

pattern ScalingActivityStatusCode_InProgress :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_InProgress = ScalingActivityStatusCode' "InProgress"

pattern ScalingActivityStatusCode_Overridden :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Overridden = ScalingActivityStatusCode' "Overridden"

pattern ScalingActivityStatusCode_Pending :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Pending = ScalingActivityStatusCode' "Pending"

pattern ScalingActivityStatusCode_Successful :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Successful = ScalingActivityStatusCode' "Successful"

pattern ScalingActivityStatusCode_Unfulfilled :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Unfulfilled = ScalingActivityStatusCode' "Unfulfilled"

{-# COMPLETE
  ScalingActivityStatusCode_Failed,
  ScalingActivityStatusCode_InProgress,
  ScalingActivityStatusCode_Overridden,
  ScalingActivityStatusCode_Pending,
  ScalingActivityStatusCode_Successful,
  ScalingActivityStatusCode_Unfulfilled,
  ScalingActivityStatusCode'
  #-}
