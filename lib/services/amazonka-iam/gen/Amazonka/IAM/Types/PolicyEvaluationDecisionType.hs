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
-- Module      : Amazonka.IAM.Types.PolicyEvaluationDecisionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyEvaluationDecisionType
  ( PolicyEvaluationDecisionType
      ( ..,
        PolicyEvaluationDecisionType_Allowed,
        PolicyEvaluationDecisionType_ExplicitDeny,
        PolicyEvaluationDecisionType_ImplicitDeny
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PolicyEvaluationDecisionType = PolicyEvaluationDecisionType'
  { fromPolicyEvaluationDecisionType ::
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
