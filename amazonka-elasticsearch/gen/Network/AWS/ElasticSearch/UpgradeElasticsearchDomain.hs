{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an Upgrade eligibility check to a compatible Elasticsearch version.
--
--
module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
    (
    -- * Creating a Request
      upgradeElasticsearchDomain
    , UpgradeElasticsearchDomain
    -- * Request Lenses
    , uedPerformCheckOnly
    , uedDomainName
    , uedTargetVersion

    -- * Destructuring the Response
    , upgradeElasticsearchDomainResponse
    , UpgradeElasticsearchDomainResponse
    -- * Response Lenses
    , uedrsDomainName
    , uedrsPerformCheckOnly
    , uedrsTargetVersion
    , uedrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'UpgradeElasticsearchDomain' @ operation.
--
--
--
-- /See:/ 'upgradeElasticsearchDomain' smart constructor.
data UpgradeElasticsearchDomain = UpgradeElasticsearchDomain'
  { _uedPerformCheckOnly :: !(Maybe Bool)
  , _uedDomainName       :: !Text
  , _uedTargetVersion    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradeElasticsearchDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedPerformCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- * 'uedDomainName' - Undocumented member.
--
-- * 'uedTargetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
upgradeElasticsearchDomain
    :: Text -- ^ 'uedDomainName'
    -> Text -- ^ 'uedTargetVersion'
    -> UpgradeElasticsearchDomain
upgradeElasticsearchDomain pDomainName_ pTargetVersion_ =
  UpgradeElasticsearchDomain'
    { _uedPerformCheckOnly = Nothing
    , _uedDomainName = pDomainName_
    , _uedTargetVersion = pTargetVersion_
    }


-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
uedPerformCheckOnly :: Lens' UpgradeElasticsearchDomain (Maybe Bool)
uedPerformCheckOnly = lens _uedPerformCheckOnly (\ s a -> s{_uedPerformCheckOnly = a})

-- | Undocumented member.
uedDomainName :: Lens' UpgradeElasticsearchDomain Text
uedDomainName = lens _uedDomainName (\ s a -> s{_uedDomainName = a})

-- | The version of Elasticsearch that you intend to upgrade the domain to.
uedTargetVersion :: Lens' UpgradeElasticsearchDomain Text
uedTargetVersion = lens _uedTargetVersion (\ s a -> s{_uedTargetVersion = a})

instance AWSRequest UpgradeElasticsearchDomain where
        type Rs UpgradeElasticsearchDomain =
             UpgradeElasticsearchDomainResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 UpgradeElasticsearchDomainResponse' <$>
                   (x .?> "DomainName") <*> (x .?> "PerformCheckOnly")
                     <*> (x .?> "TargetVersion")
                     <*> (pure (fromEnum s)))

instance Hashable UpgradeElasticsearchDomain where

instance NFData UpgradeElasticsearchDomain where

instance ToHeaders UpgradeElasticsearchDomain where
        toHeaders = const mempty

instance ToJSON UpgradeElasticsearchDomain where
        toJSON UpgradeElasticsearchDomain'{..}
          = object
              (catMaybes
                 [("PerformCheckOnly" .=) <$> _uedPerformCheckOnly,
                  Just ("DomainName" .= _uedDomainName),
                  Just ("TargetVersion" .= _uedTargetVersion)])

instance ToPath UpgradeElasticsearchDomain where
        toPath = const "/2015-01-01/es/upgradeDomain"

instance ToQuery UpgradeElasticsearchDomain where
        toQuery = const mempty

-- | Container for response returned by @'UpgradeElasticsearchDomain' @ operation.
--
--
--
-- /See:/ 'upgradeElasticsearchDomainResponse' smart constructor.
data UpgradeElasticsearchDomainResponse = UpgradeElasticsearchDomainResponse'
  { _uedrsDomainName       :: !(Maybe Text)
  , _uedrsPerformCheckOnly :: !(Maybe Bool)
  , _uedrsTargetVersion    :: !(Maybe Text)
  , _uedrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradeElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedrsDomainName' - Undocumented member.
--
-- * 'uedrsPerformCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- * 'uedrsTargetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
--
-- * 'uedrsResponseStatus' - -- | The response status code.
upgradeElasticsearchDomainResponse
    :: Int -- ^ 'uedrsResponseStatus'
    -> UpgradeElasticsearchDomainResponse
upgradeElasticsearchDomainResponse pResponseStatus_ =
  UpgradeElasticsearchDomainResponse'
    { _uedrsDomainName = Nothing
    , _uedrsPerformCheckOnly = Nothing
    , _uedrsTargetVersion = Nothing
    , _uedrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
uedrsDomainName :: Lens' UpgradeElasticsearchDomainResponse (Maybe Text)
uedrsDomainName = lens _uedrsDomainName (\ s a -> s{_uedrsDomainName = a})

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
uedrsPerformCheckOnly :: Lens' UpgradeElasticsearchDomainResponse (Maybe Bool)
uedrsPerformCheckOnly = lens _uedrsPerformCheckOnly (\ s a -> s{_uedrsPerformCheckOnly = a})

-- | The version of Elasticsearch that you intend to upgrade the domain to.
uedrsTargetVersion :: Lens' UpgradeElasticsearchDomainResponse (Maybe Text)
uedrsTargetVersion = lens _uedrsTargetVersion (\ s a -> s{_uedrsTargetVersion = a})

-- | -- | The response status code.
uedrsResponseStatus :: Lens' UpgradeElasticsearchDomainResponse Int
uedrsResponseStatus = lens _uedrsResponseStatus (\ s a -> s{_uedrsResponseStatus = a})

instance NFData UpgradeElasticsearchDomainResponse
         where
