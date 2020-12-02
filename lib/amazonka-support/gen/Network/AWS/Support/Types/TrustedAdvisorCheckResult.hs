{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
import Network.AWS.Support.Types.TrustedAdvisorResourceDetail
import Network.AWS.Support.Types.TrustedAdvisorResourcesSummary

-- | The results of a Trusted Advisor check returned by 'DescribeTrustedAdvisorCheckResult' .
--
--
--
-- /See:/ 'trustedAdvisorCheckResult' smart constructor.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult'
  { _tacrCheckId ::
      !Text,
    _tacrTimestamp :: !Text,
    _tacrStatus :: !Text,
    _tacrResourcesSummary ::
      !TrustedAdvisorResourcesSummary,
    _tacrCategorySpecificSummary ::
      !TrustedAdvisorCategorySpecificSummary,
    _tacrFlaggedResources ::
      ![TrustedAdvisorResourceDetail]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCheckResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacrCheckId' - The unique identifier for the Trusted Advisor check.
--
-- * 'tacrTimestamp' - The time of the last refresh of the check.
--
-- * 'tacrStatus' - The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- * 'tacrResourcesSummary' - Undocumented member.
--
-- * 'tacrCategorySpecificSummary' - Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
--
-- * 'tacrFlaggedResources' - The details about each resource listed in the check result.
trustedAdvisorCheckResult ::
  -- | 'tacrCheckId'
  Text ->
  -- | 'tacrTimestamp'
  Text ->
  -- | 'tacrStatus'
  Text ->
  -- | 'tacrResourcesSummary'
  TrustedAdvisorResourcesSummary ->
  -- | 'tacrCategorySpecificSummary'
  TrustedAdvisorCategorySpecificSummary ->
  TrustedAdvisorCheckResult
trustedAdvisorCheckResult
  pCheckId_
  pTimestamp_
  pStatus_
  pResourcesSummary_
  pCategorySpecificSummary_ =
    TrustedAdvisorCheckResult'
      { _tacrCheckId = pCheckId_,
        _tacrTimestamp = pTimestamp_,
        _tacrStatus = pStatus_,
        _tacrResourcesSummary = pResourcesSummary_,
        _tacrCategorySpecificSummary = pCategorySpecificSummary_,
        _tacrFlaggedResources = mempty
      }

-- | The unique identifier for the Trusted Advisor check.
tacrCheckId :: Lens' TrustedAdvisorCheckResult Text
tacrCheckId = lens _tacrCheckId (\s a -> s {_tacrCheckId = a})

-- | The time of the last refresh of the check.
tacrTimestamp :: Lens' TrustedAdvisorCheckResult Text
tacrTimestamp = lens _tacrTimestamp (\s a -> s {_tacrTimestamp = a})

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
tacrStatus :: Lens' TrustedAdvisorCheckResult Text
tacrStatus = lens _tacrStatus (\s a -> s {_tacrStatus = a})

-- | Undocumented member.
tacrResourcesSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorResourcesSummary
tacrResourcesSummary = lens _tacrResourcesSummary (\s a -> s {_tacrResourcesSummary = a})

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
tacrCategorySpecificSummary :: Lens' TrustedAdvisorCheckResult TrustedAdvisorCategorySpecificSummary
tacrCategorySpecificSummary = lens _tacrCategorySpecificSummary (\s a -> s {_tacrCategorySpecificSummary = a})

-- | The details about each resource listed in the check result.
tacrFlaggedResources :: Lens' TrustedAdvisorCheckResult [TrustedAdvisorResourceDetail]
tacrFlaggedResources = lens _tacrFlaggedResources (\s a -> s {_tacrFlaggedResources = a}) . _Coerce

instance FromJSON TrustedAdvisorCheckResult where
  parseJSON =
    withObject
      "TrustedAdvisorCheckResult"
      ( \x ->
          TrustedAdvisorCheckResult'
            <$> (x .: "checkId")
            <*> (x .: "timestamp")
            <*> (x .: "status")
            <*> (x .: "resourcesSummary")
            <*> (x .: "categorySpecificSummary")
            <*> (x .:? "flaggedResources" .!= mempty)
      )

instance Hashable TrustedAdvisorCheckResult

instance NFData TrustedAdvisorCheckResult
