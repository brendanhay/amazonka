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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ScalingActivityStatusCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalingActivityStatusCode = ScalingActivityStatusCode'
  { fromScalingActivityStatusCode ::
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
