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
-- Module      : Network.AWS.SageMaker.DescribeApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the app.
module Network.AWS.SageMaker.DescribeApp
  ( -- * Creating a Request
    describeApp,
    DescribeApp,

    -- * Request Lenses
    daDomainId,
    daUserProfileName,
    daAppType,
    daAppName,

    -- * Destructuring the Response
    describeAppResponse,
    DescribeAppResponse,

    -- * Response Lenses
    darsCreationTime,
    darsStatus,
    darsFailureReason,
    darsResourceSpec,
    darsUserProfileName,
    darsLastUserActivityTimestamp,
    darsLastHealthCheckTimestamp,
    darsAppName,
    darsAppARN,
    darsDomainId,
    darsAppType,
    darsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeApp' smart constructor.
data DescribeApp = DescribeApp'
  { _daDomainId :: !Text,
    _daUserProfileName :: !Text,
    _daAppType :: !AppType,
    _daAppName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daDomainId' - The domain ID.
--
-- * 'daUserProfileName' - The user profile name.
--
-- * 'daAppType' - The type of app.
--
-- * 'daAppName' - The name of the app.
describeApp ::
  -- | 'daDomainId'
  Text ->
  -- | 'daUserProfileName'
  Text ->
  -- | 'daAppType'
  AppType ->
  -- | 'daAppName'
  Text ->
  DescribeApp
describeApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  DescribeApp'
    { _daDomainId = pDomainId_,
      _daUserProfileName = pUserProfileName_,
      _daAppType = pAppType_,
      _daAppName = pAppName_
    }

-- | The domain ID.
daDomainId :: Lens' DescribeApp Text
daDomainId = lens _daDomainId (\s a -> s {_daDomainId = a})

-- | The user profile name.
daUserProfileName :: Lens' DescribeApp Text
daUserProfileName = lens _daUserProfileName (\s a -> s {_daUserProfileName = a})

-- | The type of app.
daAppType :: Lens' DescribeApp AppType
daAppType = lens _daAppType (\s a -> s {_daAppType = a})

-- | The name of the app.
daAppName :: Lens' DescribeApp Text
daAppName = lens _daAppName (\s a -> s {_daAppName = a})

instance AWSRequest DescribeApp where
  type Rs DescribeApp = DescribeAppResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "Status")
            <*> (x .?> "FailureReason")
            <*> (x .?> "ResourceSpec")
            <*> (x .?> "UserProfileName")
            <*> (x .?> "LastUserActivityTimestamp")
            <*> (x .?> "LastHealthCheckTimestamp")
            <*> (x .?> "AppName")
            <*> (x .?> "AppArn")
            <*> (x .?> "DomainId")
            <*> (x .?> "AppType")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeApp

instance NFData DescribeApp

instance ToHeaders DescribeApp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeApp" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeApp where
  toJSON DescribeApp' {..} =
    object
      ( catMaybes
          [ Just ("DomainId" .= _daDomainId),
            Just ("UserProfileName" .= _daUserProfileName),
            Just ("AppType" .= _daAppType),
            Just ("AppName" .= _daAppName)
          ]
      )

instance ToPath DescribeApp where
  toPath = const "/"

instance ToQuery DescribeApp where
  toQuery = const mempty

-- | /See:/ 'describeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { _darsCreationTime ::
      !(Maybe POSIX),
    _darsStatus :: !(Maybe AppStatus),
    _darsFailureReason :: !(Maybe Text),
    _darsResourceSpec :: !(Maybe ResourceSpec),
    _darsUserProfileName :: !(Maybe Text),
    _darsLastUserActivityTimestamp :: !(Maybe POSIX),
    _darsLastHealthCheckTimestamp :: !(Maybe POSIX),
    _darsAppName :: !(Maybe Text),
    _darsAppARN :: !(Maybe Text),
    _darsDomainId :: !(Maybe Text),
    _darsAppType :: !(Maybe AppType),
    _darsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsCreationTime' - The creation time.
--
-- * 'darsStatus' - The status.
--
-- * 'darsFailureReason' - The failure reason.
--
-- * 'darsResourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- * 'darsUserProfileName' - The user profile name.
--
-- * 'darsLastUserActivityTimestamp' - The timestamp of the last user's activity.
--
-- * 'darsLastHealthCheckTimestamp' - The timestamp of the last health check.
--
-- * 'darsAppName' - The name of the app.
--
-- * 'darsAppARN' - The Amazon Resource Name (ARN) of the app.
--
-- * 'darsDomainId' - The domain ID.
--
-- * 'darsAppType' - The type of app.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAppResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DescribeAppResponse
describeAppResponse pResponseStatus_ =
  DescribeAppResponse'
    { _darsCreationTime = Nothing,
      _darsStatus = Nothing,
      _darsFailureReason = Nothing,
      _darsResourceSpec = Nothing,
      _darsUserProfileName = Nothing,
      _darsLastUserActivityTimestamp = Nothing,
      _darsLastHealthCheckTimestamp = Nothing,
      _darsAppName = Nothing,
      _darsAppARN = Nothing,
      _darsDomainId = Nothing,
      _darsAppType = Nothing,
      _darsResponseStatus = pResponseStatus_
    }

-- | The creation time.
darsCreationTime :: Lens' DescribeAppResponse (Maybe UTCTime)
darsCreationTime = lens _darsCreationTime (\s a -> s {_darsCreationTime = a}) . mapping _Time

-- | The status.
darsStatus :: Lens' DescribeAppResponse (Maybe AppStatus)
darsStatus = lens _darsStatus (\s a -> s {_darsStatus = a})

-- | The failure reason.
darsFailureReason :: Lens' DescribeAppResponse (Maybe Text)
darsFailureReason = lens _darsFailureReason (\s a -> s {_darsFailureReason = a})

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
darsResourceSpec :: Lens' DescribeAppResponse (Maybe ResourceSpec)
darsResourceSpec = lens _darsResourceSpec (\s a -> s {_darsResourceSpec = a})

-- | The user profile name.
darsUserProfileName :: Lens' DescribeAppResponse (Maybe Text)
darsUserProfileName = lens _darsUserProfileName (\s a -> s {_darsUserProfileName = a})

-- | The timestamp of the last user's activity.
darsLastUserActivityTimestamp :: Lens' DescribeAppResponse (Maybe UTCTime)
darsLastUserActivityTimestamp = lens _darsLastUserActivityTimestamp (\s a -> s {_darsLastUserActivityTimestamp = a}) . mapping _Time

-- | The timestamp of the last health check.
darsLastHealthCheckTimestamp :: Lens' DescribeAppResponse (Maybe UTCTime)
darsLastHealthCheckTimestamp = lens _darsLastHealthCheckTimestamp (\s a -> s {_darsLastHealthCheckTimestamp = a}) . mapping _Time

-- | The name of the app.
darsAppName :: Lens' DescribeAppResponse (Maybe Text)
darsAppName = lens _darsAppName (\s a -> s {_darsAppName = a})

-- | The Amazon Resource Name (ARN) of the app.
darsAppARN :: Lens' DescribeAppResponse (Maybe Text)
darsAppARN = lens _darsAppARN (\s a -> s {_darsAppARN = a})

-- | The domain ID.
darsDomainId :: Lens' DescribeAppResponse (Maybe Text)
darsDomainId = lens _darsDomainId (\s a -> s {_darsDomainId = a})

-- | The type of app.
darsAppType :: Lens' DescribeAppResponse (Maybe AppType)
darsAppType = lens _darsAppType (\s a -> s {_darsAppType = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAppResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DescribeAppResponse
