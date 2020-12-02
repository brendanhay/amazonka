{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.Container where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types.ContainerStatus
import Network.AWS.Prelude

-- | This section describes operations that you can perform on an AWS Elemental MediaStore container.
--
--
--
-- /See:/ 'container' smart constructor.
data Container = Container'
  { _cCreationTime :: !(Maybe POSIX),
    _cStatus :: !(Maybe ContainerStatus),
    _cAccessLoggingEnabled :: !(Maybe Bool),
    _cARN :: !(Maybe Text),
    _cName :: !(Maybe Text),
    _cEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - Unix timestamp.
--
-- * 'cStatus' - The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
--
-- * 'cAccessLoggingEnabled' - The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
--
-- * 'cARN' - The Amazon Resource Name (ARN) of the container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>  For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- * 'cName' - The name of the container.
--
-- * 'cEndpoint' - The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
container ::
  Container
container =
  Container'
    { _cCreationTime = Nothing,
      _cStatus = Nothing,
      _cAccessLoggingEnabled = Nothing,
      _cARN = Nothing,
      _cName = Nothing,
      _cEndpoint = Nothing
    }

-- | Unix timestamp.
cCreationTime :: Lens' Container (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\s a -> s {_cCreationTime = a}) . mapping _Time

-- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
cStatus :: Lens' Container (Maybe ContainerStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
cAccessLoggingEnabled :: Lens' Container (Maybe Bool)
cAccessLoggingEnabled = lens _cAccessLoggingEnabled (\s a -> s {_cAccessLoggingEnabled = a})

-- | The Amazon Resource Name (ARN) of the container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>  For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
cARN :: Lens' Container (Maybe Text)
cARN = lens _cARN (\s a -> s {_cARN = a})

-- | The name of the container.
cName :: Lens' Container (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
cEndpoint :: Lens' Container (Maybe Text)
cEndpoint = lens _cEndpoint (\s a -> s {_cEndpoint = a})

instance FromJSON Container where
  parseJSON =
    withObject
      "Container"
      ( \x ->
          Container'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "AccessLoggingEnabled")
            <*> (x .:? "ARN")
            <*> (x .:? "Name")
            <*> (x .:? "Endpoint")
      )

instance Hashable Container

instance NFData Container
