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

import qualified Network.AWS.Prelude as Prelude

newtype PolicyEvaluationDecisionType = PolicyEvaluationDecisionType'
  { fromPolicyEvaluationDecisionType ::
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
