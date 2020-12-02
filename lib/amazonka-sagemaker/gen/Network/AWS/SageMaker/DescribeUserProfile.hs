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
-- Module      : Network.AWS.SageMaker.DescribeUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user profile. For more information, see @CreateUserProfile@ .
module Network.AWS.SageMaker.DescribeUserProfile
  ( -- * Creating a Request
    describeUserProfile,
    DescribeUserProfile,

    -- * Request Lenses
    dupDomainId,
    dupUserProfileName,

    -- * Destructuring the Response
    describeUserProfileResponse,
    DescribeUserProfileResponse,

    -- * Response Lenses
    duprsCreationTime,
    duprsUserSettings,
    duprsStatus,
    duprsFailureReason,
    duprsSingleSignOnUserValue,
    duprsUserProfileName,
    duprsLastModifiedTime,
    duprsHomeEfsFileSystemUid,
    duprsUserProfileARN,
    duprsSingleSignOnUserIdentifier,
    duprsDomainId,
    duprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { _dupDomainId ::
      !Text,
    _dupUserProfileName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupDomainId' - The domain ID.
--
-- * 'dupUserProfileName' - The user profile name.
describeUserProfile ::
  -- | 'dupDomainId'
  Text ->
  -- | 'dupUserProfileName'
  Text ->
  DescribeUserProfile
describeUserProfile pDomainId_ pUserProfileName_ =
  DescribeUserProfile'
    { _dupDomainId = pDomainId_,
      _dupUserProfileName = pUserProfileName_
    }

-- | The domain ID.
dupDomainId :: Lens' DescribeUserProfile Text
dupDomainId = lens _dupDomainId (\s a -> s {_dupDomainId = a})

-- | The user profile name.
dupUserProfileName :: Lens' DescribeUserProfile Text
dupUserProfileName = lens _dupUserProfileName (\s a -> s {_dupUserProfileName = a})

instance AWSRequest DescribeUserProfile where
  type Rs DescribeUserProfile = DescribeUserProfileResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "UserSettings")
            <*> (x .?> "Status")
            <*> (x .?> "FailureReason")
            <*> (x .?> "SingleSignOnUserValue")
            <*> (x .?> "UserProfileName")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "HomeEfsFileSystemUid")
            <*> (x .?> "UserProfileArn")
            <*> (x .?> "SingleSignOnUserIdentifier")
            <*> (x .?> "DomainId")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeUserProfile

instance NFData DescribeUserProfile

instance ToHeaders DescribeUserProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeUserProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    object
      ( catMaybes
          [ Just ("DomainId" .= _dupDomainId),
            Just ("UserProfileName" .= _dupUserProfileName)
          ]
      )

instance ToPath DescribeUserProfile where
  toPath = const "/"

instance ToQuery DescribeUserProfile where
  toQuery = const mempty

-- | /See:/ 'describeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { _duprsCreationTime ::
      !(Maybe POSIX),
    _duprsUserSettings ::
      !(Maybe UserSettings),
    _duprsStatus ::
      !(Maybe UserProfileStatus),
    _duprsFailureReason ::
      !(Maybe Text),
    _duprsSingleSignOnUserValue ::
      !(Maybe Text),
    _duprsUserProfileName ::
      !(Maybe Text),
    _duprsLastModifiedTime ::
      !(Maybe POSIX),
    _duprsHomeEfsFileSystemUid ::
      !(Maybe Text),
    _duprsUserProfileARN ::
      !(Maybe Text),
    _duprsSingleSignOnUserIdentifier ::
      !(Maybe Text),
    _duprsDomainId :: !(Maybe Text),
    _duprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duprsCreationTime' - The creation time.
--
-- * 'duprsUserSettings' - A collection of settings.
--
-- * 'duprsStatus' - The status.
--
-- * 'duprsFailureReason' - The failure reason.
--
-- * 'duprsSingleSignOnUserValue' - The SSO user value.
--
-- * 'duprsUserProfileName' - The user profile name.
--
-- * 'duprsLastModifiedTime' - The last modified time.
--
-- * 'duprsHomeEfsFileSystemUid' - The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
--
-- * 'duprsUserProfileARN' - The user profile Amazon Resource Name (ARN).
--
-- * 'duprsSingleSignOnUserIdentifier' - The SSO user identifier.
--
-- * 'duprsDomainId' - The ID of the domain that contains the profile.
--
-- * 'duprsResponseStatus' - -- | The response status code.
describeUserProfileResponse ::
  -- | 'duprsResponseStatus'
  Int ->
  DescribeUserProfileResponse
describeUserProfileResponse pResponseStatus_ =
  DescribeUserProfileResponse'
    { _duprsCreationTime = Nothing,
      _duprsUserSettings = Nothing,
      _duprsStatus = Nothing,
      _duprsFailureReason = Nothing,
      _duprsSingleSignOnUserValue = Nothing,
      _duprsUserProfileName = Nothing,
      _duprsLastModifiedTime = Nothing,
      _duprsHomeEfsFileSystemUid = Nothing,
      _duprsUserProfileARN = Nothing,
      _duprsSingleSignOnUserIdentifier = Nothing,
      _duprsDomainId = Nothing,
      _duprsResponseStatus = pResponseStatus_
    }

-- | The creation time.
duprsCreationTime :: Lens' DescribeUserProfileResponse (Maybe UTCTime)
duprsCreationTime = lens _duprsCreationTime (\s a -> s {_duprsCreationTime = a}) . mapping _Time

-- | A collection of settings.
duprsUserSettings :: Lens' DescribeUserProfileResponse (Maybe UserSettings)
duprsUserSettings = lens _duprsUserSettings (\s a -> s {_duprsUserSettings = a})

-- | The status.
duprsStatus :: Lens' DescribeUserProfileResponse (Maybe UserProfileStatus)
duprsStatus = lens _duprsStatus (\s a -> s {_duprsStatus = a})

-- | The failure reason.
duprsFailureReason :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsFailureReason = lens _duprsFailureReason (\s a -> s {_duprsFailureReason = a})

-- | The SSO user value.
duprsSingleSignOnUserValue :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsSingleSignOnUserValue = lens _duprsSingleSignOnUserValue (\s a -> s {_duprsSingleSignOnUserValue = a})

-- | The user profile name.
duprsUserProfileName :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsUserProfileName = lens _duprsUserProfileName (\s a -> s {_duprsUserProfileName = a})

-- | The last modified time.
duprsLastModifiedTime :: Lens' DescribeUserProfileResponse (Maybe UTCTime)
duprsLastModifiedTime = lens _duprsLastModifiedTime (\s a -> s {_duprsLastModifiedTime = a}) . mapping _Time

-- | The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
duprsHomeEfsFileSystemUid :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsHomeEfsFileSystemUid = lens _duprsHomeEfsFileSystemUid (\s a -> s {_duprsHomeEfsFileSystemUid = a})

-- | The user profile Amazon Resource Name (ARN).
duprsUserProfileARN :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsUserProfileARN = lens _duprsUserProfileARN (\s a -> s {_duprsUserProfileARN = a})

-- | The SSO user identifier.
duprsSingleSignOnUserIdentifier :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsSingleSignOnUserIdentifier = lens _duprsSingleSignOnUserIdentifier (\s a -> s {_duprsSingleSignOnUserIdentifier = a})

-- | The ID of the domain that contains the profile.
duprsDomainId :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsDomainId = lens _duprsDomainId (\s a -> s {_duprsDomainId = a})

-- | -- | The response status code.
duprsResponseStatus :: Lens' DescribeUserProfileResponse Int
duprsResponseStatus = lens _duprsResponseStatus (\s a -> s {_duprsResponseStatus = a})

instance NFData DescribeUserProfileResponse
