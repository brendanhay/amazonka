{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorResourcesSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor 'DescribeTrustedAdvisorCheckSummaries' .
--
--
--
-- /See:/ 'trustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
  { _tarsResourcesProcessed ::
      !Integer,
    _tarsResourcesFlagged ::
      !Integer,
    _tarsResourcesIgnored ::
      !Integer,
    _tarsResourcesSuppressed ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorResourcesSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tarsResourcesProcessed' - The number of AWS resources that were analyzed by the Trusted Advisor check.
--
-- * 'tarsResourcesFlagged' - The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
--
-- * 'tarsResourcesIgnored' - The number of AWS resources ignored by Trusted Advisor because information was unavailable.
--
-- * 'tarsResourcesSuppressed' - The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
trustedAdvisorResourcesSummary ::
  -- | 'tarsResourcesProcessed'
  Integer ->
  -- | 'tarsResourcesFlagged'
  Integer ->
  -- | 'tarsResourcesIgnored'
  Integer ->
  -- | 'tarsResourcesSuppressed'
  Integer ->
  TrustedAdvisorResourcesSummary
trustedAdvisorResourcesSummary
  pResourcesProcessed_
  pResourcesFlagged_
  pResourcesIgnored_
  pResourcesSuppressed_ =
    TrustedAdvisorResourcesSummary'
      { _tarsResourcesProcessed =
          pResourcesProcessed_,
        _tarsResourcesFlagged = pResourcesFlagged_,
        _tarsResourcesIgnored = pResourcesIgnored_,
        _tarsResourcesSuppressed = pResourcesSuppressed_
      }

-- | The number of AWS resources that were analyzed by the Trusted Advisor check.
tarsResourcesProcessed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesProcessed = lens _tarsResourcesProcessed (\s a -> s {_tarsResourcesProcessed = a})

-- | The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
tarsResourcesFlagged :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesFlagged = lens _tarsResourcesFlagged (\s a -> s {_tarsResourcesFlagged = a})

-- | The number of AWS resources ignored by Trusted Advisor because information was unavailable.
tarsResourcesIgnored :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesIgnored = lens _tarsResourcesIgnored (\s a -> s {_tarsResourcesIgnored = a})

-- | The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
tarsResourcesSuppressed :: Lens' TrustedAdvisorResourcesSummary Integer
tarsResourcesSuppressed = lens _tarsResourcesSuppressed (\s a -> s {_tarsResourcesSuppressed = a})

instance FromJSON TrustedAdvisorResourcesSummary where
  parseJSON =
    withObject
      "TrustedAdvisorResourcesSummary"
      ( \x ->
          TrustedAdvisorResourcesSummary'
            <$> (x .: "resourcesProcessed")
            <*> (x .: "resourcesFlagged")
            <*> (x .: "resourcesIgnored")
            <*> (x .: "resourcesSuppressed")
      )

instance Hashable TrustedAdvisorResourcesSummary

instance NFData TrustedAdvisorResourcesSummary
