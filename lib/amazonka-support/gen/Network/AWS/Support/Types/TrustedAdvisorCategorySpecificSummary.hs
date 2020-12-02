{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary

-- | The container for summary information that relates to the category of the Trusted Advisor check.
--
--
--
-- /See:/ 'trustedAdvisorCategorySpecificSummary' smart constructor.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
  { _tacssCostOptimizing ::
      Maybe
        TrustedAdvisorCostOptimizingSummary
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCategorySpecificSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacssCostOptimizing' - The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
trustedAdvisorCategorySpecificSummary ::
  TrustedAdvisorCategorySpecificSummary
trustedAdvisorCategorySpecificSummary =
  TrustedAdvisorCategorySpecificSummary'
    { _tacssCostOptimizing =
        Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
tacssCostOptimizing :: Lens' TrustedAdvisorCategorySpecificSummary (Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing = lens _tacssCostOptimizing (\s a -> s {_tacssCostOptimizing = a})

instance FromJSON TrustedAdvisorCategorySpecificSummary where
  parseJSON =
    withObject
      "TrustedAdvisorCategorySpecificSummary"
      ( \x ->
          TrustedAdvisorCategorySpecificSummary'
            <$> (x .:? "costOptimizing")
      )

instance Hashable TrustedAdvisorCategorySpecificSummary

instance NFData TrustedAdvisorCategorySpecificSummary
