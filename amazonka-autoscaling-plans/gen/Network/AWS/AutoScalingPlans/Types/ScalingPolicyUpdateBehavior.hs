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
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
  ( ScalingPolicyUpdateBehavior
      ( ..,
        ScalingPolicyUpdateBehavior_KeepExternalPolicies,
        ScalingPolicyUpdateBehavior_ReplaceExternalPolicies
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScalingPolicyUpdateBehavior = ScalingPolicyUpdateBehavior'
  { fromScalingPolicyUpdateBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ScalingPolicyUpdateBehavior_KeepExternalPolicies :: ScalingPolicyUpdateBehavior
pattern ScalingPolicyUpdateBehavior_KeepExternalPolicies = ScalingPolicyUpdateBehavior' "KeepExternalPolicies"

pattern ScalingPolicyUpdateBehavior_ReplaceExternalPolicies :: ScalingPolicyUpdateBehavior
pattern ScalingPolicyUpdateBehavior_ReplaceExternalPolicies = ScalingPolicyUpdateBehavior' "ReplaceExternalPolicies"

{-# COMPLETE
  ScalingPolicyUpdateBehavior_KeepExternalPolicies,
  ScalingPolicyUpdateBehavior_ReplaceExternalPolicies,
  ScalingPolicyUpdateBehavior'
  #-}
