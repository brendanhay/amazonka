{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
import Network.AWS.Support.Types.TrustedAdvisorResourcesSummary

-- | A summary of a Trusted Advisor check result, including the alert status, last refresh, and number of resources examined.
--
--
--
-- /See:/ 'trustedAdvisorCheckSummary' smart constructor.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary'
  { _tacsHasFlaggedResources ::
      !(Maybe Bool),
    _tacsCheckId :: !Text,
    _tacsTimestamp :: !Text,
    _tacsStatus :: !Text,
    _tacsResourcesSummary ::
      !TrustedAdvisorResourcesSummary,
    _tacsCategorySpecificSummary ::
      !TrustedAdvisorCategorySpecificSummary
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCheckSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacsHasFlaggedResources' - Specifies whether the Trusted Advisor check has flagged resources.
--
-- * 'tacsCheckId' - The unique identifier for the Trusted Advisor check.
--
-- * 'tacsTimestamp' - The time of the last refresh of the check.
--
-- * 'tacsStatus' - The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- * 'tacsResourcesSummary' - Undocumented member.
--
-- * 'tacsCategorySpecificSummary' - Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
trustedAdvisorCheckSummary ::
  -- | 'tacsCheckId'
  Text ->
  -- | 'tacsTimestamp'
  Text ->
  -- | 'tacsStatus'
  Text ->
  -- | 'tacsResourcesSummary'
  TrustedAdvisorResourcesSummary ->
  -- | 'tacsCategorySpecificSummary'
  TrustedAdvisorCategorySpecificSummary ->
  TrustedAdvisorCheckSummary
trustedAdvisorCheckSummary
  pCheckId_
  pTimestamp_
  pStatus_
  pResourcesSummary_
  pCategorySpecificSummary_ =
    TrustedAdvisorCheckSummary'
      { _tacsHasFlaggedResources = Nothing,
        _tacsCheckId = pCheckId_,
        _tacsTimestamp = pTimestamp_,
        _tacsStatus = pStatus_,
        _tacsResourcesSummary = pResourcesSummary_,
        _tacsCategorySpecificSummary = pCategorySpecificSummary_
      }

-- | Specifies whether the Trusted Advisor check has flagged resources.
tacsHasFlaggedResources :: Lens' TrustedAdvisorCheckSummary (Maybe Bool)
tacsHasFlaggedResources = lens _tacsHasFlaggedResources (\s a -> s {_tacsHasFlaggedResources = a})

-- | The unique identifier for the Trusted Advisor check.
tacsCheckId :: Lens' TrustedAdvisorCheckSummary Text
tacsCheckId = lens _tacsCheckId (\s a -> s {_tacsCheckId = a})

-- | The time of the last refresh of the check.
tacsTimestamp :: Lens' TrustedAdvisorCheckSummary Text
tacsTimestamp = lens _tacsTimestamp (\s a -> s {_tacsTimestamp = a})

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
tacsStatus :: Lens' TrustedAdvisorCheckSummary Text
tacsStatus = lens _tacsStatus (\s a -> s {_tacsStatus = a})

-- | Undocumented member.
tacsResourcesSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary = lens _tacsResourcesSummary (\s a -> s {_tacsResourcesSummary = a})

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
tacsCategorySpecificSummary :: Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary = lens _tacsCategorySpecificSummary (\s a -> s {_tacsCategorySpecificSummary = a})

instance FromJSON TrustedAdvisorCheckSummary where
  parseJSON =
    withObject
      "TrustedAdvisorCheckSummary"
      ( \x ->
          TrustedAdvisorCheckSummary'
            <$> (x .:? "hasFlaggedResources")
            <*> (x .: "checkId")
            <*> (x .: "timestamp")
            <*> (x .: "status")
            <*> (x .: "resourcesSummary")
            <*> (x .: "categorySpecificSummary")
      )

instance Hashable TrustedAdvisorCheckSummary

instance NFData TrustedAdvisorCheckSummary
