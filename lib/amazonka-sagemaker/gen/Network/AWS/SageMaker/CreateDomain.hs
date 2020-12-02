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
-- Module      : Network.AWS.SageMaker.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Domain@ used by Amazon SageMaker Studio. A domain consists of an associated Amazon Elastic File System (EFS) volume, a list of authorized users, and a variety of security, application, policy, and Amazon Virtual Private Cloud (VPC) configurations. An AWS account is limited to one domain per region. Users within a domain can share notebook files and other artifacts with each other.
--
--
-- When a domain is created, an EFS volume is created for use by all of the users within the domain. Each user receives a private home directory within the EFS volume for notebooks, Git repositories, and data files.
--
-- __VPC configuration__
--
-- All SageMaker Studio traffic between the domain and the EFS volume is through the specified VPC and subnets. For other Studio traffic, you can specify the @AppNetworkAccessType@ parameter. @AppNetworkAccessType@ corresponds to the network access type that you choose when you onboard to Studio. The following options are available:
--
--     * @PublicInternetOnly@ - Non-EFS traffic goes through a VPC managed by Amazon SageMaker, which allows internet access. This is the default value.
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets. Internet access is disabled by default. To allow internet access, you must specify a NAT gateway.
--
-- When internet access is disabled, you won't be able to run a Studio notebook or to train or host models unless your VPC has an interface endpoint to the SageMaker API and runtime or a NAT gateway and your security groups allow outbound connections.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-notebooks-and-internet-access.html Connect SageMaker Studio Notebooks to Resources in a VPC> .
module Network.AWS.SageMaker.CreateDomain
  ( -- * Creating a Request
    createDomain,
    CreateDomain,

    -- * Request Lenses
    cdHomeEfsFileSystemKMSKeyId,
    cdKMSKeyId,
    cdAppNetworkAccessType,
    cdTags,
    cdDomainName,
    cdAuthMode,
    cdDefaultUserSettings,
    cdSubnetIds,
    cdVPCId,

    -- * Destructuring the Response
    createDomainResponse,
    CreateDomainResponse,

    -- * Response Lenses
    cdrsDomainARN,
    cdrsURL,
    cdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createDomain' smart constructor.
data CreateDomain = CreateDomain'
  { _cdHomeEfsFileSystemKMSKeyId ::
      !(Maybe Text),
    _cdKMSKeyId :: !(Maybe Text),
    _cdAppNetworkAccessType :: !(Maybe AppNetworkAccessType),
    _cdTags :: !(Maybe [Tag]),
    _cdDomainName :: !Text,
    _cdAuthMode :: !AuthMode,
    _cdDefaultUserSettings :: !UserSettings,
    _cdSubnetIds :: !(List1 Text),
    _cdVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdHomeEfsFileSystemKMSKeyId' - This member is deprecated and replaced with @KmsKeyId@ .
--
-- * 'cdKMSKeyId' - SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
--
-- * 'cdAppNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
-- * 'cdTags' - Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
--
-- * 'cdDomainName' - A name for the domain.
--
-- * 'cdAuthMode' - The mode of authentication that members use to access the domain.
--
-- * 'cdDefaultUserSettings' - The default user settings.
--
-- * 'cdSubnetIds' - The VPC subnets that Studio uses for communication.
--
-- * 'cdVPCId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
createDomain ::
  -- | 'cdDomainName'
  Text ->
  -- | 'cdAuthMode'
  AuthMode ->
  -- | 'cdDefaultUserSettings'
  UserSettings ->
  -- | 'cdSubnetIds'
  NonEmpty Text ->
  -- | 'cdVPCId'
  Text ->
  CreateDomain
createDomain
  pDomainName_
  pAuthMode_
  pDefaultUserSettings_
  pSubnetIds_
  pVPCId_ =
    CreateDomain'
      { _cdHomeEfsFileSystemKMSKeyId = Nothing,
        _cdKMSKeyId = Nothing,
        _cdAppNetworkAccessType = Nothing,
        _cdTags = Nothing,
        _cdDomainName = pDomainName_,
        _cdAuthMode = pAuthMode_,
        _cdDefaultUserSettings = pDefaultUserSettings_,
        _cdSubnetIds = _List1 # pSubnetIds_,
        _cdVPCId = pVPCId_
      }

-- | This member is deprecated and replaced with @KmsKeyId@ .
cdHomeEfsFileSystemKMSKeyId :: Lens' CreateDomain (Maybe Text)
cdHomeEfsFileSystemKMSKeyId = lens _cdHomeEfsFileSystemKMSKeyId (\s a -> s {_cdHomeEfsFileSystemKMSKeyId = a})

-- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
cdKMSKeyId :: Lens' CreateDomain (Maybe Text)
cdKMSKeyId = lens _cdKMSKeyId (\s a -> s {_cdKMSKeyId = a})

-- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
cdAppNetworkAccessType :: Lens' CreateDomain (Maybe AppNetworkAccessType)
cdAppNetworkAccessType = lens _cdAppNetworkAccessType (\s a -> s {_cdAppNetworkAccessType = a})

-- | Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
cdTags :: Lens' CreateDomain [Tag]
cdTags = lens _cdTags (\s a -> s {_cdTags = a}) . _Default . _Coerce

-- | A name for the domain.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\s a -> s {_cdDomainName = a})

-- | The mode of authentication that members use to access the domain.
cdAuthMode :: Lens' CreateDomain AuthMode
cdAuthMode = lens _cdAuthMode (\s a -> s {_cdAuthMode = a})

-- | The default user settings.
cdDefaultUserSettings :: Lens' CreateDomain UserSettings
cdDefaultUserSettings = lens _cdDefaultUserSettings (\s a -> s {_cdDefaultUserSettings = a})

-- | The VPC subnets that Studio uses for communication.
cdSubnetIds :: Lens' CreateDomain (NonEmpty Text)
cdSubnetIds = lens _cdSubnetIds (\s a -> s {_cdSubnetIds = a}) . _List1

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
cdVPCId :: Lens' CreateDomain Text
cdVPCId = lens _cdVPCId (\s a -> s {_cdVPCId = a})

instance AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            <$> (x .?> "DomainArn") <*> (x .?> "Url") <*> (pure (fromEnum s))
      )

instance Hashable CreateDomain

instance NFData CreateDomain

instance ToHeaders CreateDomain where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateDomain" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    object
      ( catMaybes
          [ ("HomeEfsFileSystemKmsKeyId" .=) <$> _cdHomeEfsFileSystemKMSKeyId,
            ("KmsKeyId" .=) <$> _cdKMSKeyId,
            ("AppNetworkAccessType" .=) <$> _cdAppNetworkAccessType,
            ("Tags" .=) <$> _cdTags,
            Just ("DomainName" .= _cdDomainName),
            Just ("AuthMode" .= _cdAuthMode),
            Just ("DefaultUserSettings" .= _cdDefaultUserSettings),
            Just ("SubnetIds" .= _cdSubnetIds),
            Just ("VpcId" .= _cdVPCId)
          ]
      )

instance ToPath CreateDomain where
  toPath = const "/"

instance ToQuery CreateDomain where
  toQuery = const mempty

-- | /See:/ 'createDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { _cdrsDomainARN ::
      !(Maybe Text),
    _cdrsURL :: !(Maybe Text),
    _cdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDomainARN' - The Amazon Resource Name (ARN) of the created domain.
--
-- * 'cdrsURL' - The URL to the created domain.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDomainResponse ::
  -- | 'cdrsResponseStatus'
  Int ->
  CreateDomainResponse
createDomainResponse pResponseStatus_ =
  CreateDomainResponse'
    { _cdrsDomainARN = Nothing,
      _cdrsURL = Nothing,
      _cdrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the created domain.
cdrsDomainARN :: Lens' CreateDomainResponse (Maybe Text)
cdrsDomainARN = lens _cdrsDomainARN (\s a -> s {_cdrsDomainARN = a})

-- | The URL to the created domain.
cdrsURL :: Lens' CreateDomainResponse (Maybe Text)
cdrsURL = lens _cdrsURL (\s a -> s {_cdrsURL = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDomainResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\s a -> s {_cdrsResponseStatus = a})

instance NFData CreateDomainResponse
