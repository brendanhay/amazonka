{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyEvaluationDecisionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyEvaluationDecisionType where

import Network.AWS.Prelude

data PolicyEvaluationDecisionType
  = Allowed
  | ExplicitDeny
  | ImplicitDeny
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PolicyEvaluationDecisionType where
  parser =
    takeLowerText >>= \case
      "allowed" -> pure Allowed
      "explicitdeny" -> pure ExplicitDeny
      "implicitdeny" -> pure ImplicitDeny
      e ->
        fromTextError $
          "Failure parsing PolicyEvaluationDecisionType from value: '" <> e
            <> "'. Accepted values: allowed, explicitdeny, implicitdeny"

instance ToText PolicyEvaluationDecisionType where
  toText = \case
    Allowed -> "allowed"
    ExplicitDeny -> "explicitDeny"
    ImplicitDeny -> "implicitDeny"

instance Hashable PolicyEvaluationDecisionType

instance NFData PolicyEvaluationDecisionType

instance ToByteString PolicyEvaluationDecisionType

instance ToQuery PolicyEvaluationDecisionType

instance ToHeader PolicyEvaluationDecisionType

instance FromXML PolicyEvaluationDecisionType where
  parseXML = parseXMLText "PolicyEvaluationDecisionType"
