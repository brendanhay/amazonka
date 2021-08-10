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
-- Module      : Network.AWS.IAM.Types.PolicyEvaluationDecisionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyEvaluationDecisionType
  ( PolicyEvaluationDecisionType
      ( ..,
        PolicyEvaluationDecisionType_Allowed,
        PolicyEvaluationDecisionType_ExplicitDeny,
        PolicyEvaluationDecisionType_ImplicitDeny
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PolicyEvaluationDecisionType = PolicyEvaluationDecisionType'
  { fromPolicyEvaluationDecisionType ::
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

pattern PolicyEvaluationDecisionType_Allowed :: PolicyEvaluationDecisionType
pattern PolicyEvaluationDecisionType_Allowed = PolicyEvaluationDecisionType' "allowed"

pattern PolicyEvaluationDecisionType_ExplicitDeny :: PolicyEvaluationDecisionType
pattern PolicyEvaluationDecisionType_ExplicitDeny = PolicyEvaluationDecisionType' "explicitDeny"

pattern PolicyEvaluationDecisionType_ImplicitDeny :: PolicyEvaluationDecisionType
pattern PolicyEvaluationDecisionType_ImplicitDeny = PolicyEvaluationDecisionType' "implicitDeny"

{-# COMPLETE
  PolicyEvaluationDecisionType_Allowed,
  PolicyEvaluationDecisionType_ExplicitDeny,
  PolicyEvaluationDecisionType_ImplicitDeny,
  PolicyEvaluationDecisionType'
  #-}
