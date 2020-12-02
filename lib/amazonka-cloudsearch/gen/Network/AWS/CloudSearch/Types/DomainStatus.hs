{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainStatus where

import Network.AWS.CloudSearch.Types.Limits
import Network.AWS.CloudSearch.Types.ServiceEndpoint
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The current status of the search domain.
--
--
--
-- /See:/ 'domainStatus' smart constructor.
data DomainStatus = DomainStatus'
  { _dsSearchInstanceCount ::
      !(Maybe Nat),
    _dsSearchInstanceType :: !(Maybe Text),
    _dsDocService :: !(Maybe ServiceEndpoint),
    _dsARN :: !(Maybe Text),
    _dsCreated :: !(Maybe Bool),
    _dsSearchService :: !(Maybe ServiceEndpoint),
    _dsLimits :: !(Maybe Limits),
    _dsSearchPartitionCount :: !(Maybe Nat),
    _dsDeleted :: !(Maybe Bool),
    _dsProcessing :: !(Maybe Bool),
    _dsDomainId :: !Text,
    _dsDomainName :: !Text,
    _dsRequiresIndexDocuments :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSearchInstanceCount' - The number of search instances that are available to process search requests.
--
-- * 'dsSearchInstanceType' - The instance type that is being used to process search requests.
--
-- * 'dsDocService' - The service endpoint for updating documents in a search domain.
--
-- * 'dsARN' - Undocumented member.
--
-- * 'dsCreated' - True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
--
-- * 'dsSearchService' - The service endpoint for requesting search results from a search domain.
--
-- * 'dsLimits' - Undocumented member.
--
-- * 'dsSearchPartitionCount' - The number of partitions across which the search index is spread.
--
-- * 'dsDeleted' - True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
--
-- * 'dsProcessing' - True if processing is being done to activate the current domain configuration.
--
-- * 'dsDomainId' - Undocumented member.
--
-- * 'dsDomainName' - Undocumented member.
--
-- * 'dsRequiresIndexDocuments' - True if 'IndexDocuments' needs to be called to activate the current domain configuration.
domainStatus ::
  -- | 'dsDomainId'
  Text ->
  -- | 'dsDomainName'
  Text ->
  -- | 'dsRequiresIndexDocuments'
  Bool ->
  DomainStatus
domainStatus pDomainId_ pDomainName_ pRequiresIndexDocuments_ =
  DomainStatus'
    { _dsSearchInstanceCount = Nothing,
      _dsSearchInstanceType = Nothing,
      _dsDocService = Nothing,
      _dsARN = Nothing,
      _dsCreated = Nothing,
      _dsSearchService = Nothing,
      _dsLimits = Nothing,
      _dsSearchPartitionCount = Nothing,
      _dsDeleted = Nothing,
      _dsProcessing = Nothing,
      _dsDomainId = pDomainId_,
      _dsDomainName = pDomainName_,
      _dsRequiresIndexDocuments = pRequiresIndexDocuments_
    }

-- | The number of search instances that are available to process search requests.
dsSearchInstanceCount :: Lens' DomainStatus (Maybe Natural)
dsSearchInstanceCount = lens _dsSearchInstanceCount (\s a -> s {_dsSearchInstanceCount = a}) . mapping _Nat

-- | The instance type that is being used to process search requests.
dsSearchInstanceType :: Lens' DomainStatus (Maybe Text)
dsSearchInstanceType = lens _dsSearchInstanceType (\s a -> s {_dsSearchInstanceType = a})

-- | The service endpoint for updating documents in a search domain.
dsDocService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsDocService = lens _dsDocService (\s a -> s {_dsDocService = a})

-- | Undocumented member.
dsARN :: Lens' DomainStatus (Maybe Text)
dsARN = lens _dsARN (\s a -> s {_dsARN = a})

-- | True if the search domain is created. It can take several minutes to initialize a domain when 'CreateDomain' is called. Newly created search domains are returned from 'DescribeDomains' with a false value for Created until domain creation is complete.
dsCreated :: Lens' DomainStatus (Maybe Bool)
dsCreated = lens _dsCreated (\s a -> s {_dsCreated = a})

-- | The service endpoint for requesting search results from a search domain.
dsSearchService :: Lens' DomainStatus (Maybe ServiceEndpoint)
dsSearchService = lens _dsSearchService (\s a -> s {_dsSearchService = a})

-- | Undocumented member.
dsLimits :: Lens' DomainStatus (Maybe Limits)
dsLimits = lens _dsLimits (\s a -> s {_dsLimits = a})

-- | The number of partitions across which the search index is spread.
dsSearchPartitionCount :: Lens' DomainStatus (Maybe Natural)
dsSearchPartitionCount = lens _dsSearchPartitionCount (\s a -> s {_dsSearchPartitionCount = a}) . mapping _Nat

-- | True if the search domain has been deleted. The system must clean up resources dedicated to the search domain when 'DeleteDomain' is called. Newly deleted search domains are returned from 'DescribeDomains' with a true value for IsDeleted for several minutes until resource cleanup is complete.
dsDeleted :: Lens' DomainStatus (Maybe Bool)
dsDeleted = lens _dsDeleted (\s a -> s {_dsDeleted = a})

-- | True if processing is being done to activate the current domain configuration.
dsProcessing :: Lens' DomainStatus (Maybe Bool)
dsProcessing = lens _dsProcessing (\s a -> s {_dsProcessing = a})

-- | Undocumented member.
dsDomainId :: Lens' DomainStatus Text
dsDomainId = lens _dsDomainId (\s a -> s {_dsDomainId = a})

-- | Undocumented member.
dsDomainName :: Lens' DomainStatus Text
dsDomainName = lens _dsDomainName (\s a -> s {_dsDomainName = a})

-- | True if 'IndexDocuments' needs to be called to activate the current domain configuration.
dsRequiresIndexDocuments :: Lens' DomainStatus Bool
dsRequiresIndexDocuments = lens _dsRequiresIndexDocuments (\s a -> s {_dsRequiresIndexDocuments = a})

instance FromXML DomainStatus where
  parseXML x =
    DomainStatus'
      <$> (x .@? "SearchInstanceCount")
      <*> (x .@? "SearchInstanceType")
      <*> (x .@? "DocService")
      <*> (x .@? "ARN")
      <*> (x .@? "Created")
      <*> (x .@? "SearchService")
      <*> (x .@? "Limits")
      <*> (x .@? "SearchPartitionCount")
      <*> (x .@? "Deleted")
      <*> (x .@? "Processing")
      <*> (x .@ "DomainId")
      <*> (x .@ "DomainName")
      <*> (x .@ "RequiresIndexDocuments")

instance Hashable DomainStatus

instance NFData DomainStatus
