{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The refresh status of a Trusted Advisor check.
--
--
--
-- /See:/ 'trustedAdvisorCheckRefreshStatus' smart constructor.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus'
  { _tacrsCheckId ::
      !Text,
    _tacrsStatus :: !Text,
    _tacrsMillisUntilNextRefreshable ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCheckRefreshStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacrsCheckId' - The unique identifier for the Trusted Advisor check.
--
-- * 'tacrsStatus' - The status of the Trusted Advisor check for which a refresh has been requested:      * @none:@ The check is not refreshed or the non-success status exceeds the timeout     * @enqueued:@ The check refresh requests has entered the refresh queue     * @processing:@ The check refresh request is picked up by the rule processing engine     * @success:@ The check is successfully refreshed     * @abandoned:@ The check refresh has failed
--
-- * 'tacrsMillisUntilNextRefreshable' - The amount of time, in milliseconds, until the Trusted Advisor check is eligible for refresh.
trustedAdvisorCheckRefreshStatus ::
  -- | 'tacrsCheckId'
  Text ->
  -- | 'tacrsStatus'
  Text ->
  -- | 'tacrsMillisUntilNextRefreshable'
  Integer ->
  TrustedAdvisorCheckRefreshStatus
trustedAdvisorCheckRefreshStatus
  pCheckId_
  pStatus_
  pMillisUntilNextRefreshable_ =
    TrustedAdvisorCheckRefreshStatus'
      { _tacrsCheckId = pCheckId_,
        _tacrsStatus = pStatus_,
        _tacrsMillisUntilNextRefreshable =
          pMillisUntilNextRefreshable_
      }

-- | The unique identifier for the Trusted Advisor check.
tacrsCheckId :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsCheckId = lens _tacrsCheckId (\s a -> s {_tacrsCheckId = a})

-- | The status of the Trusted Advisor check for which a refresh has been requested:      * @none:@ The check is not refreshed or the non-success status exceeds the timeout     * @enqueued:@ The check refresh requests has entered the refresh queue     * @processing:@ The check refresh request is picked up by the rule processing engine     * @success:@ The check is successfully refreshed     * @abandoned:@ The check refresh has failed
tacrsStatus :: Lens' TrustedAdvisorCheckRefreshStatus Text
tacrsStatus = lens _tacrsStatus (\s a -> s {_tacrsStatus = a})

-- | The amount of time, in milliseconds, until the Trusted Advisor check is eligible for refresh.
tacrsMillisUntilNextRefreshable :: Lens' TrustedAdvisorCheckRefreshStatus Integer
tacrsMillisUntilNextRefreshable = lens _tacrsMillisUntilNextRefreshable (\s a -> s {_tacrsMillisUntilNextRefreshable = a})

instance FromJSON TrustedAdvisorCheckRefreshStatus where
  parseJSON =
    withObject
      "TrustedAdvisorCheckRefreshStatus"
      ( \x ->
          TrustedAdvisorCheckRefreshStatus'
            <$> (x .: "checkId")
            <*> (x .: "status")
            <*> (x .: "millisUntilNextRefreshable")
      )

instance Hashable TrustedAdvisorCheckRefreshStatus

instance NFData TrustedAdvisorCheckRefreshStatus
