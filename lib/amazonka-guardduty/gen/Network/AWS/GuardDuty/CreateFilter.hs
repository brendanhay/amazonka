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
-- Module      : Network.AWS.GuardDuty.CreateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a filter using the specified finding criteria.
module Network.AWS.GuardDuty.CreateFilter
  ( -- * Creating a Request
    createFilter,
    CreateFilter,

    -- * Request Lenses
    cfClientToken,
    cfAction,
    cfDescription,
    cfRank,
    cfTags,
    cfDetectorId,
    cfName,
    cfFindingCriteria,

    -- * Destructuring the Response
    createFilterResponse,
    CreateFilterResponse,

    -- * Response Lenses
    cfrsResponseStatus,
    cfrsName,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFilter' smart constructor.
data CreateFilter = CreateFilter'
  { _cfClientToken :: !(Maybe Text),
    _cfAction :: !(Maybe FilterAction),
    _cfDescription :: !(Maybe Text),
    _cfRank :: !(Maybe Nat),
    _cfTags :: !(Maybe (Map Text (Text))),
    _cfDetectorId :: !Text,
    _cfName :: !Text,
    _cfFindingCriteria :: !FindingCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfClientToken' - The idempotency token for the create request.
--
-- * 'cfAction' - Specifies the action that is to be applied to the findings that match the filter.
--
-- * 'cfDescription' - The description of the filter.
--
-- * 'cfRank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- * 'cfTags' - The tags to be added to a new filter resource.
--
-- * 'cfDetectorId' - The unique ID of the detector of the GuardDuty account that you want to create a filter for.
--
-- * 'cfName' - The name of the filter.
--
-- * 'cfFindingCriteria' - Represents the criteria to be used in the filter for querying findings. You can only use the following attributes to query findings:     * accountId     * region     * confidence     * id     * resource.accessKeyDetails.accessKeyId     * resource.accessKeyDetails.principalId     * resource.accessKeyDetails.userName     * resource.accessKeyDetails.userType     * resource.instanceDetails.iamInstanceProfile.id     * resource.instanceDetails.imageId     * resource.instanceDetails.instanceId     * resource.instanceDetails.outpostArn     * resource.instanceDetails.networkInterfaces.ipv6Addresses     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress     * resource.instanceDetails.networkInterfaces.publicDnsName     * resource.instanceDetails.networkInterfaces.publicIp     * resource.instanceDetails.networkInterfaces.securityGroups.groupId     * resource.instanceDetails.networkInterfaces.securityGroups.groupName     * resource.instanceDetails.networkInterfaces.subnetId     * resource.instanceDetails.networkInterfaces.vpcId     * resource.instanceDetails.tags.key     * resource.instanceDetails.tags.value     * resource.resourceType     * service.action.actionType     * service.action.awsApiCallAction.api     * service.action.awsApiCallAction.callerType     * service.action.awsApiCallAction.remoteIpDetails.city.cityName     * service.action.awsApiCallAction.remoteIpDetails.country.countryName     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4     * service.action.awsApiCallAction.remoteIpDetails.organization.asn     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg     * service.action.awsApiCallAction.serviceName     * service.action.dnsRequestAction.domain     * service.action.networkConnectionAction.blocked     * service.action.networkConnectionAction.connectionDirection     * service.action.networkConnectionAction.localPortDetails.port     * service.action.networkConnectionAction.protocol     * service.action.networkConnectionAction.localIpDetails.ipAddressV4     * service.action.networkConnectionAction.remoteIpDetails.city.cityName     * service.action.networkConnectionAction.remoteIpDetails.country.countryName     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4     * service.action.networkConnectionAction.remoteIpDetails.organization.asn     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg     * service.action.networkConnectionAction.remotePortDetails.port     * service.additionalInfo.threatListName     * service.archived When this attribute is set to TRUE, only archived findings are listed. When it's set to FALSE, only unarchived findings are listed. When this attribute is not set, all existing findings are listed.     * service.resourceRole     * severity     * type     * updatedAt Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains milliseconds.
createFilter ::
  -- | 'cfDetectorId'
  Text ->
  -- | 'cfName'
  Text ->
  -- | 'cfFindingCriteria'
  FindingCriteria ->
  CreateFilter
createFilter pDetectorId_ pName_ pFindingCriteria_ =
  CreateFilter'
    { _cfClientToken = Nothing,
      _cfAction = Nothing,
      _cfDescription = Nothing,
      _cfRank = Nothing,
      _cfTags = Nothing,
      _cfDetectorId = pDetectorId_,
      _cfName = pName_,
      _cfFindingCriteria = pFindingCriteria_
    }

-- | The idempotency token for the create request.
cfClientToken :: Lens' CreateFilter (Maybe Text)
cfClientToken = lens _cfClientToken (\s a -> s {_cfClientToken = a})

-- | Specifies the action that is to be applied to the findings that match the filter.
cfAction :: Lens' CreateFilter (Maybe FilterAction)
cfAction = lens _cfAction (\s a -> s {_cfAction = a})

-- | The description of the filter.
cfDescription :: Lens' CreateFilter (Maybe Text)
cfDescription = lens _cfDescription (\s a -> s {_cfDescription = a})

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
cfRank :: Lens' CreateFilter (Maybe Natural)
cfRank = lens _cfRank (\s a -> s {_cfRank = a}) . mapping _Nat

-- | The tags to be added to a new filter resource.
cfTags :: Lens' CreateFilter (HashMap Text (Text))
cfTags = lens _cfTags (\s a -> s {_cfTags = a}) . _Default . _Map

-- | The unique ID of the detector of the GuardDuty account that you want to create a filter for.
cfDetectorId :: Lens' CreateFilter Text
cfDetectorId = lens _cfDetectorId (\s a -> s {_cfDetectorId = a})

-- | The name of the filter.
cfName :: Lens' CreateFilter Text
cfName = lens _cfName (\s a -> s {_cfName = a})

-- | Represents the criteria to be used in the filter for querying findings. You can only use the following attributes to query findings:     * accountId     * region     * confidence     * id     * resource.accessKeyDetails.accessKeyId     * resource.accessKeyDetails.principalId     * resource.accessKeyDetails.userName     * resource.accessKeyDetails.userType     * resource.instanceDetails.iamInstanceProfile.id     * resource.instanceDetails.imageId     * resource.instanceDetails.instanceId     * resource.instanceDetails.outpostArn     * resource.instanceDetails.networkInterfaces.ipv6Addresses     * resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress     * resource.instanceDetails.networkInterfaces.publicDnsName     * resource.instanceDetails.networkInterfaces.publicIp     * resource.instanceDetails.networkInterfaces.securityGroups.groupId     * resource.instanceDetails.networkInterfaces.securityGroups.groupName     * resource.instanceDetails.networkInterfaces.subnetId     * resource.instanceDetails.networkInterfaces.vpcId     * resource.instanceDetails.tags.key     * resource.instanceDetails.tags.value     * resource.resourceType     * service.action.actionType     * service.action.awsApiCallAction.api     * service.action.awsApiCallAction.callerType     * service.action.awsApiCallAction.remoteIpDetails.city.cityName     * service.action.awsApiCallAction.remoteIpDetails.country.countryName     * service.action.awsApiCallAction.remoteIpDetails.ipAddressV4     * service.action.awsApiCallAction.remoteIpDetails.organization.asn     * service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg     * service.action.awsApiCallAction.serviceName     * service.action.dnsRequestAction.domain     * service.action.networkConnectionAction.blocked     * service.action.networkConnectionAction.connectionDirection     * service.action.networkConnectionAction.localPortDetails.port     * service.action.networkConnectionAction.protocol     * service.action.networkConnectionAction.localIpDetails.ipAddressV4     * service.action.networkConnectionAction.remoteIpDetails.city.cityName     * service.action.networkConnectionAction.remoteIpDetails.country.countryName     * service.action.networkConnectionAction.remoteIpDetails.ipAddressV4     * service.action.networkConnectionAction.remoteIpDetails.organization.asn     * service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg     * service.action.networkConnectionAction.remotePortDetails.port     * service.additionalInfo.threatListName     * service.archived When this attribute is set to TRUE, only archived findings are listed. When it's set to FALSE, only unarchived findings are listed. When this attribute is not set, all existing findings are listed.     * service.resourceRole     * severity     * type     * updatedAt Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains milliseconds.
cfFindingCriteria :: Lens' CreateFilter FindingCriteria
cfFindingCriteria = lens _cfFindingCriteria (\s a -> s {_cfFindingCriteria = a})

instance AWSRequest CreateFilter where
  type Rs CreateFilter = CreateFilterResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          CreateFilterResponse' <$> (pure (fromEnum s)) <*> (x .:> "name")
      )

instance Hashable CreateFilter

instance NFData CreateFilter

instance ToHeaders CreateFilter where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateFilter where
  toJSON CreateFilter' {..} =
    object
      ( catMaybes
          [ ("clientToken" .=) <$> _cfClientToken,
            ("action" .=) <$> _cfAction,
            ("description" .=) <$> _cfDescription,
            ("rank" .=) <$> _cfRank,
            ("tags" .=) <$> _cfTags,
            Just ("name" .= _cfName),
            Just ("findingCriteria" .= _cfFindingCriteria)
          ]
      )

instance ToPath CreateFilter where
  toPath CreateFilter' {..} =
    mconcat ["/detector/", toBS _cfDetectorId, "/filter"]

instance ToQuery CreateFilter where
  toQuery = const mempty

-- | /See:/ 'createFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { _cfrsResponseStatus ::
      !Int,
    _cfrsName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsResponseStatus' - -- | The response status code.
--
-- * 'cfrsName' - The name of the successfully created filter.
createFilterResponse ::
  -- | 'cfrsResponseStatus'
  Int ->
  -- | 'cfrsName'
  Text ->
  CreateFilterResponse
createFilterResponse pResponseStatus_ pName_ =
  CreateFilterResponse'
    { _cfrsResponseStatus = pResponseStatus_,
      _cfrsName = pName_
    }

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFilterResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\s a -> s {_cfrsResponseStatus = a})

-- | The name of the successfully created filter.
cfrsName :: Lens' CreateFilterResponse Text
cfrsName = lens _cfrsName (\s a -> s {_cfrsName = a})

instance NFData CreateFilterResponse
