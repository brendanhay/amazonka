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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
  ( ScalingPolicyUpdateBehavior
      ( ..,
        ScalingPolicyUpdateBehavior_KeepExternalPolicies,
        ScalingPolicyUpdateBehavior_ReplaceExternalPolicies
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalingPolicyUpdateBehavior = ScalingPolicyUpdateBehavior'
  { fromScalingPolicyUpdateBehavior ::
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

pattern ScalingPolicyUpdateBehavior_KeepExternalPolicies :: ScalingPolicyUpdateBehavior
pattern ScalingPolicyUpdateBehavior_KeepExternalPolicies = ScalingPolicyUpdateBehavior' "KeepExternalPolicies"

pattern ScalingPolicyUpdateBehavior_ReplaceExternalPolicies :: ScalingPolicyUpdateBehavior
pattern ScalingPolicyUpdateBehavior_ReplaceExternalPolicies = ScalingPolicyUpdateBehavior' "ReplaceExternalPolicies"

{-# COMPLETE
  ScalingPolicyUpdateBehavior_KeepExternalPolicies,
  ScalingPolicyUpdateBehavior_ReplaceExternalPolicies,
  ScalingPolicyUpdateBehavior'
  #-}
