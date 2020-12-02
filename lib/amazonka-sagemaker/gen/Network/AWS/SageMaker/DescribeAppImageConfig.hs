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
-- Module      : Network.AWS.SageMaker.DescribeAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an AppImageConfig.
module Network.AWS.SageMaker.DescribeAppImageConfig
  ( -- * Creating a Request
    describeAppImageConfig,
    DescribeAppImageConfig,

    -- * Request Lenses
    dAppImageConfigName,

    -- * Destructuring the Response
    describeAppImageConfigResponse,
    DescribeAppImageConfigResponse,

    -- * Response Lenses
    daicrsCreationTime,
    daicrsAppImageConfigName,
    daicrsLastModifiedTime,
    daicrsKernelGatewayImageConfig,
    daicrsAppImageConfigARN,
    daicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeAppImageConfig' smart constructor.
newtype DescribeAppImageConfig = DescribeAppImageConfig'
  { _dAppImageConfigName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAppImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAppImageConfigName' - The name of the AppImageConfig to describe.
describeAppImageConfig ::
  -- | 'dAppImageConfigName'
  Text ->
  DescribeAppImageConfig
describeAppImageConfig pAppImageConfigName_ =
  DescribeAppImageConfig'
    { _dAppImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to describe.
dAppImageConfigName :: Lens' DescribeAppImageConfig Text
dAppImageConfigName = lens _dAppImageConfigName (\s a -> s {_dAppImageConfigName = a})

instance AWSRequest DescribeAppImageConfig where
  type Rs DescribeAppImageConfig = DescribeAppImageConfigResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeAppImageConfigResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "AppImageConfigName")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "KernelGatewayImageConfig")
            <*> (x .?> "AppImageConfigArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAppImageConfig

instance NFData DescribeAppImageConfig

instance ToHeaders DescribeAppImageConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeAppImageConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAppImageConfig where
  toJSON DescribeAppImageConfig' {..} =
    object
      (catMaybes [Just ("AppImageConfigName" .= _dAppImageConfigName)])

instance ToPath DescribeAppImageConfig where
  toPath = const "/"

instance ToQuery DescribeAppImageConfig where
  toQuery = const mempty

-- | /See:/ 'describeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { _daicrsCreationTime ::
      !(Maybe POSIX),
    _daicrsAppImageConfigName ::
      !(Maybe Text),
    _daicrsLastModifiedTime ::
      !(Maybe POSIX),
    _daicrsKernelGatewayImageConfig ::
      !( Maybe
           KernelGatewayImageConfig
       ),
    _daicrsAppImageConfigARN ::
      !(Maybe Text),
    _daicrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAppImageConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daicrsCreationTime' - When the AppImageConfig was created.
--
-- * 'daicrsAppImageConfigName' - The name of the AppImageConfig.
--
-- * 'daicrsLastModifiedTime' - When the AppImageConfig was last modified.
--
-- * 'daicrsKernelGatewayImageConfig' - The configuration of a KernelGateway app.
--
-- * 'daicrsAppImageConfigARN' - The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- * 'daicrsResponseStatus' - -- | The response status code.
describeAppImageConfigResponse ::
  -- | 'daicrsResponseStatus'
  Int ->
  DescribeAppImageConfigResponse
describeAppImageConfigResponse pResponseStatus_ =
  DescribeAppImageConfigResponse'
    { _daicrsCreationTime = Nothing,
      _daicrsAppImageConfigName = Nothing,
      _daicrsLastModifiedTime = Nothing,
      _daicrsKernelGatewayImageConfig = Nothing,
      _daicrsAppImageConfigARN = Nothing,
      _daicrsResponseStatus = pResponseStatus_
    }

-- | When the AppImageConfig was created.
daicrsCreationTime :: Lens' DescribeAppImageConfigResponse (Maybe UTCTime)
daicrsCreationTime = lens _daicrsCreationTime (\s a -> s {_daicrsCreationTime = a}) . mapping _Time

-- | The name of the AppImageConfig.
daicrsAppImageConfigName :: Lens' DescribeAppImageConfigResponse (Maybe Text)
daicrsAppImageConfigName = lens _daicrsAppImageConfigName (\s a -> s {_daicrsAppImageConfigName = a})

-- | When the AppImageConfig was last modified.
daicrsLastModifiedTime :: Lens' DescribeAppImageConfigResponse (Maybe UTCTime)
daicrsLastModifiedTime = lens _daicrsLastModifiedTime (\s a -> s {_daicrsLastModifiedTime = a}) . mapping _Time

-- | The configuration of a KernelGateway app.
daicrsKernelGatewayImageConfig :: Lens' DescribeAppImageConfigResponse (Maybe KernelGatewayImageConfig)
daicrsKernelGatewayImageConfig = lens _daicrsKernelGatewayImageConfig (\s a -> s {_daicrsKernelGatewayImageConfig = a})

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
daicrsAppImageConfigARN :: Lens' DescribeAppImageConfigResponse (Maybe Text)
daicrsAppImageConfigARN = lens _daicrsAppImageConfigARN (\s a -> s {_daicrsAppImageConfigARN = a})

-- | -- | The response status code.
daicrsResponseStatus :: Lens' DescribeAppImageConfigResponse Int
daicrsResponseStatus = lens _daicrsResponseStatus (\s a -> s {_daicrsResponseStatus = a})

instance NFData DescribeAppImageConfigResponse
