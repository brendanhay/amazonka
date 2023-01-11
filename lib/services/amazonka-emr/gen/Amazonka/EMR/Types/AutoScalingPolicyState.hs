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
-- Module      : Amazonka.EMR.Types.AutoScalingPolicyState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoScalingPolicyState
  ( AutoScalingPolicyState
      ( ..,
        AutoScalingPolicyState_ATTACHED,
        AutoScalingPolicyState_ATTACHING,
        AutoScalingPolicyState_DETACHED,
        AutoScalingPolicyState_DETACHING,
        AutoScalingPolicyState_FAILED,
        AutoScalingPolicyState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoScalingPolicyState = AutoScalingPolicyState'
  { fromAutoScalingPolicyState ::
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

pattern AutoScalingPolicyState_ATTACHED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_ATTACHED = AutoScalingPolicyState' "ATTACHED"

pattern AutoScalingPolicyState_ATTACHING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_ATTACHING = AutoScalingPolicyState' "ATTACHING"

pattern AutoScalingPolicyState_DETACHED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_DETACHED = AutoScalingPolicyState' "DETACHED"

pattern AutoScalingPolicyState_DETACHING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_DETACHING = AutoScalingPolicyState' "DETACHING"

pattern AutoScalingPolicyState_FAILED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_FAILED = AutoScalingPolicyState' "FAILED"

pattern AutoScalingPolicyState_PENDING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_PENDING = AutoScalingPolicyState' "PENDING"

{-# COMPLETE
  AutoScalingPolicyState_ATTACHED,
  AutoScalingPolicyState_ATTACHING,
  AutoScalingPolicyState_DETACHED,
  AutoScalingPolicyState_DETACHING,
  AutoScalingPolicyState_FAILED,
  AutoScalingPolicyState_PENDING,
  AutoScalingPolicyState'
  #-}
