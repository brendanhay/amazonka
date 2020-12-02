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
-- Module      : Network.AWS.SageMaker.DescribeDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The description of the domain.
module Network.AWS.SageMaker.DescribeDomain
  ( -- * Creating a Request
    describeDomain,
    DescribeDomain,

    -- * Request Lenses
    desDomainId,

    -- * Destructuring the Response
    describeDomainResponse,
    DescribeDomainResponse,

    -- * Response Lenses
    ddrsCreationTime,
    ddrsStatus,
    ddrsFailureReason,
    ddrsDefaultUserSettings,
    ddrsSubnetIds,
    ddrsDomainARN,
    ddrsVPCId,
    ddrsURL,
    ddrsAuthMode,
    ddrsHomeEfsFileSystemKMSKeyId,
    ddrsSingleSignOnManagedApplicationInstanceId,
    ddrsLastModifiedTime,
    ddrsHomeEfsFileSystemId,
    ddrsKMSKeyId,
    ddrsDomainName,
    ddrsDomainId,
    ddrsAppNetworkAccessType,
    ddrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain' {_desDomainId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desDomainId' - The domain ID.
describeDomain ::
  -- | 'desDomainId'
  Text ->
  DescribeDomain
describeDomain pDomainId_ =
  DescribeDomain' {_desDomainId = pDomainId_}

-- | The domain ID.
desDomainId :: Lens' DescribeDomain Text
desDomainId = lens _desDomainId (\s a -> s {_desDomainId = a})

instance AWSRequest DescribeDomain where
  type Rs DescribeDomain = DescribeDomainResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "Status")
            <*> (x .?> "FailureReason")
            <*> (x .?> "DefaultUserSettings")
            <*> (x .?> "SubnetIds")
            <*> (x .?> "DomainArn")
            <*> (x .?> "VpcId")
            <*> (x .?> "Url")
            <*> (x .?> "AuthMode")
            <*> (x .?> "HomeEfsFileSystemKmsKeyId")
            <*> (x .?> "SingleSignOnManagedApplicationInstanceId")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "HomeEfsFileSystemId")
            <*> (x .?> "KmsKeyId")
            <*> (x .?> "DomainName")
            <*> (x .?> "DomainId")
            <*> (x .?> "AppNetworkAccessType")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDomain

instance NFData DescribeDomain

instance ToHeaders DescribeDomain where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeDomain" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    object (catMaybes [Just ("DomainId" .= _desDomainId)])

instance ToPath DescribeDomain where
  toPath = const "/"

instance ToQuery DescribeDomain where
  toQuery = const mempty

-- | /See:/ 'describeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { _ddrsCreationTime ::
      !(Maybe POSIX),
    _ddrsStatus :: !(Maybe DomainStatus),
    _ddrsFailureReason :: !(Maybe Text),
    _ddrsDefaultUserSettings ::
      !(Maybe UserSettings),
    _ddrsSubnetIds :: !(Maybe (List1 Text)),
    _ddrsDomainARN :: !(Maybe Text),
    _ddrsVPCId :: !(Maybe Text),
    _ddrsURL :: !(Maybe Text),
    _ddrsAuthMode :: !(Maybe AuthMode),
    _ddrsHomeEfsFileSystemKMSKeyId ::
      !(Maybe Text),
    _ddrsSingleSignOnManagedApplicationInstanceId ::
      !(Maybe Text),
    _ddrsLastModifiedTime :: !(Maybe POSIX),
    _ddrsHomeEfsFileSystemId :: !(Maybe Text),
    _ddrsKMSKeyId :: !(Maybe Text),
    _ddrsDomainName :: !(Maybe Text),
    _ddrsDomainId :: !(Maybe Text),
    _ddrsAppNetworkAccessType ::
      !(Maybe AppNetworkAccessType),
    _ddrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsCreationTime' - The creation time.
--
-- * 'ddrsStatus' - The status.
--
-- * 'ddrsFailureReason' - The failure reason.
--
-- * 'ddrsDefaultUserSettings' - Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
--
-- * 'ddrsSubnetIds' - The VPC subnets that Studio uses for communication.
--
-- * 'ddrsDomainARN' - The domain's Amazon Resource Name (ARN).
--
-- * 'ddrsVPCId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- * 'ddrsURL' - The domain's URL.
--
-- * 'ddrsAuthMode' - The domain's authentication mode.
--
-- * 'ddrsHomeEfsFileSystemKMSKeyId' - This member is deprecated and replaced with @KmsKeyId@ .
--
-- * 'ddrsSingleSignOnManagedApplicationInstanceId' - The SSO managed application instance ID.
--
-- * 'ddrsLastModifiedTime' - The last modified time.
--
-- * 'ddrsHomeEfsFileSystemId' - The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- * 'ddrsKMSKeyId' - The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
--
-- * 'ddrsDomainName' - The domain name.
--
-- * 'ddrsDomainId' - The domain ID.
--
-- * 'ddrsAppNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDomainResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DescribeDomainResponse
describeDomainResponse pResponseStatus_ =
  DescribeDomainResponse'
    { _ddrsCreationTime = Nothing,
      _ddrsStatus = Nothing,
      _ddrsFailureReason = Nothing,
      _ddrsDefaultUserSettings = Nothing,
      _ddrsSubnetIds = Nothing,
      _ddrsDomainARN = Nothing,
      _ddrsVPCId = Nothing,
      _ddrsURL = Nothing,
      _ddrsAuthMode = Nothing,
      _ddrsHomeEfsFileSystemKMSKeyId = Nothing,
      _ddrsSingleSignOnManagedApplicationInstanceId = Nothing,
      _ddrsLastModifiedTime = Nothing,
      _ddrsHomeEfsFileSystemId = Nothing,
      _ddrsKMSKeyId = Nothing,
      _ddrsDomainName = Nothing,
      _ddrsDomainId = Nothing,
      _ddrsAppNetworkAccessType = Nothing,
      _ddrsResponseStatus = pResponseStatus_
    }

-- | The creation time.
ddrsCreationTime :: Lens' DescribeDomainResponse (Maybe UTCTime)
ddrsCreationTime = lens _ddrsCreationTime (\s a -> s {_ddrsCreationTime = a}) . mapping _Time

-- | The status.
ddrsStatus :: Lens' DescribeDomainResponse (Maybe DomainStatus)
ddrsStatus = lens _ddrsStatus (\s a -> s {_ddrsStatus = a})

-- | The failure reason.
ddrsFailureReason :: Lens' DescribeDomainResponse (Maybe Text)
ddrsFailureReason = lens _ddrsFailureReason (\s a -> s {_ddrsFailureReason = a})

-- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
ddrsDefaultUserSettings :: Lens' DescribeDomainResponse (Maybe UserSettings)
ddrsDefaultUserSettings = lens _ddrsDefaultUserSettings (\s a -> s {_ddrsDefaultUserSettings = a})

-- | The VPC subnets that Studio uses for communication.
ddrsSubnetIds :: Lens' DescribeDomainResponse (Maybe (NonEmpty Text))
ddrsSubnetIds = lens _ddrsSubnetIds (\s a -> s {_ddrsSubnetIds = a}) . mapping _List1

-- | The domain's Amazon Resource Name (ARN).
ddrsDomainARN :: Lens' DescribeDomainResponse (Maybe Text)
ddrsDomainARN = lens _ddrsDomainARN (\s a -> s {_ddrsDomainARN = a})

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
ddrsVPCId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsVPCId = lens _ddrsVPCId (\s a -> s {_ddrsVPCId = a})

-- | The domain's URL.
ddrsURL :: Lens' DescribeDomainResponse (Maybe Text)
ddrsURL = lens _ddrsURL (\s a -> s {_ddrsURL = a})

-- | The domain's authentication mode.
ddrsAuthMode :: Lens' DescribeDomainResponse (Maybe AuthMode)
ddrsAuthMode = lens _ddrsAuthMode (\s a -> s {_ddrsAuthMode = a})

-- | This member is deprecated and replaced with @KmsKeyId@ .
ddrsHomeEfsFileSystemKMSKeyId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsHomeEfsFileSystemKMSKeyId = lens _ddrsHomeEfsFileSystemKMSKeyId (\s a -> s {_ddrsHomeEfsFileSystemKMSKeyId = a})

-- | The SSO managed application instance ID.
ddrsSingleSignOnManagedApplicationInstanceId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsSingleSignOnManagedApplicationInstanceId = lens _ddrsSingleSignOnManagedApplicationInstanceId (\s a -> s {_ddrsSingleSignOnManagedApplicationInstanceId = a})

-- | The last modified time.
ddrsLastModifiedTime :: Lens' DescribeDomainResponse (Maybe UTCTime)
ddrsLastModifiedTime = lens _ddrsLastModifiedTime (\s a -> s {_ddrsLastModifiedTime = a}) . mapping _Time

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
ddrsHomeEfsFileSystemId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsHomeEfsFileSystemId = lens _ddrsHomeEfsFileSystemId (\s a -> s {_ddrsHomeEfsFileSystemId = a})

-- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
ddrsKMSKeyId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsKMSKeyId = lens _ddrsKMSKeyId (\s a -> s {_ddrsKMSKeyId = a})

-- | The domain name.
ddrsDomainName :: Lens' DescribeDomainResponse (Maybe Text)
ddrsDomainName = lens _ddrsDomainName (\s a -> s {_ddrsDomainName = a})

-- | The domain ID.
ddrsDomainId :: Lens' DescribeDomainResponse (Maybe Text)
ddrsDomainId = lens _ddrsDomainId (\s a -> s {_ddrsDomainId = a})

-- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
ddrsAppNetworkAccessType :: Lens' DescribeDomainResponse (Maybe AppNetworkAccessType)
ddrsAppNetworkAccessType = lens _ddrsAppNetworkAccessType (\s a -> s {_ddrsAppNetworkAccessType = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDomainResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DescribeDomainResponse
