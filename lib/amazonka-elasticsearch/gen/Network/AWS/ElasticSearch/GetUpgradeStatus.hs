{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest status of the last upgrade or upgrade eligibility check that was performed on the domain.
module Network.AWS.ElasticSearch.GetUpgradeStatus
  ( -- * Creating a Request
    getUpgradeStatus,
    GetUpgradeStatus,

    -- * Request Lenses
    gusDomainName,

    -- * Destructuring the Response
    getUpgradeStatusResponse,
    GetUpgradeStatusResponse,

    -- * Response Lenses
    gusrsStepStatus,
    gusrsUpgradeName,
    gusrsUpgradeStep,
    gusrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'GetUpgradeStatus' @ operation.
--
--
--
-- /See:/ 'getUpgradeStatus' smart constructor.
newtype GetUpgradeStatus = GetUpgradeStatus'
  { _gusDomainName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUpgradeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gusDomainName' - Undocumented member.
getUpgradeStatus ::
  -- | 'gusDomainName'
  Text ->
  GetUpgradeStatus
getUpgradeStatus pDomainName_ =
  GetUpgradeStatus' {_gusDomainName = pDomainName_}

-- | Undocumented member.
gusDomainName :: Lens' GetUpgradeStatus Text
gusDomainName = lens _gusDomainName (\s a -> s {_gusDomainName = a})

instance AWSRequest GetUpgradeStatus where
  type Rs GetUpgradeStatus = GetUpgradeStatusResponse
  request = get elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          GetUpgradeStatusResponse'
            <$> (x .?> "StepStatus")
            <*> (x .?> "UpgradeName")
            <*> (x .?> "UpgradeStep")
            <*> (pure (fromEnum s))
      )

instance Hashable GetUpgradeStatus

instance NFData GetUpgradeStatus

instance ToHeaders GetUpgradeStatus where
  toHeaders = const mempty

instance ToPath GetUpgradeStatus where
  toPath GetUpgradeStatus' {..} =
    mconcat
      ["/2015-01-01/es/upgradeDomain/", toBS _gusDomainName, "/status"]

instance ToQuery GetUpgradeStatus where
  toQuery = const mempty

-- | Container for response returned by @'GetUpgradeStatus' @ operation.
--
--
--
-- /See:/ 'getUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { _gusrsStepStatus ::
      !(Maybe UpgradeStatus),
    _gusrsUpgradeName :: !(Maybe Text),
    _gusrsUpgradeStep :: !(Maybe UpgradeStep),
    _gusrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetUpgradeStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gusrsStepStatus' - One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
--
-- * 'gusrsUpgradeName' - A string that describes the update briefly
--
-- * 'gusrsUpgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:     * PreUpgradeCheck    * Snapshot    * Upgrade
--
-- * 'gusrsResponseStatus' - -- | The response status code.
getUpgradeStatusResponse ::
  -- | 'gusrsResponseStatus'
  Int ->
  GetUpgradeStatusResponse
getUpgradeStatusResponse pResponseStatus_ =
  GetUpgradeStatusResponse'
    { _gusrsStepStatus = Nothing,
      _gusrsUpgradeName = Nothing,
      _gusrsUpgradeStep = Nothing,
      _gusrsResponseStatus = pResponseStatus_
    }

-- | One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
gusrsStepStatus :: Lens' GetUpgradeStatusResponse (Maybe UpgradeStatus)
gusrsStepStatus = lens _gusrsStepStatus (\s a -> s {_gusrsStepStatus = a})

-- | A string that describes the update briefly
gusrsUpgradeName :: Lens' GetUpgradeStatusResponse (Maybe Text)
gusrsUpgradeName = lens _gusrsUpgradeName (\s a -> s {_gusrsUpgradeName = a})

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:     * PreUpgradeCheck    * Snapshot    * Upgrade
gusrsUpgradeStep :: Lens' GetUpgradeStatusResponse (Maybe UpgradeStep)
gusrsUpgradeStep = lens _gusrsUpgradeStep (\s a -> s {_gusrsUpgradeStep = a})

-- | -- | The response status code.
gusrsResponseStatus :: Lens' GetUpgradeStatusResponse Int
gusrsResponseStatus = lens _gusrsResponseStatus (\s a -> s {_gusrsResponseStatus = a})

instance NFData GetUpgradeStatusResponse
