{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey where

import Network.AWS.Prelude

data ConfigRuleComplianceSummaryGroupKey
  = AWSRegion
  | AccountId
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

instance FromText ConfigRuleComplianceSummaryGroupKey where
  parser =
    takeLowerText >>= \case
      "aws_region" -> pure AWSRegion
      "account_id" -> pure AccountId
      e ->
        fromTextError $
          "Failure parsing ConfigRuleComplianceSummaryGroupKey from value: '" <> e
            <> "'. Accepted values: aws_region, account_id"

instance ToText ConfigRuleComplianceSummaryGroupKey where
  toText = \case
    AWSRegion -> "AWS_REGION"
    AccountId -> "ACCOUNT_ID"

instance Hashable ConfigRuleComplianceSummaryGroupKey

instance NFData ConfigRuleComplianceSummaryGroupKey

instance ToByteString ConfigRuleComplianceSummaryGroupKey

instance ToQuery ConfigRuleComplianceSummaryGroupKey

instance ToHeader ConfigRuleComplianceSummaryGroupKey

instance ToJSON ConfigRuleComplianceSummaryGroupKey where
  toJSON = toJSONText
