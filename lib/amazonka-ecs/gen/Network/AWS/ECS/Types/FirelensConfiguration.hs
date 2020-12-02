{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FirelensConfiguration where

import Network.AWS.ECS.Types.FirelensConfigurationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'firelensConfiguration' smart constructor.
data FirelensConfiguration = FirelensConfiguration'
  { _fcOptions ::
      !(Maybe (Map Text (Text))),
    _fcType :: !FirelensConfigurationType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FirelensConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcOptions' - The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'fcType' - The log router to use. The valid values are @fluentd@ or @fluentbit@ .
firelensConfiguration ::
  -- | 'fcType'
  FirelensConfigurationType ->
  FirelensConfiguration
firelensConfiguration pType_ =
  FirelensConfiguration' {_fcOptions = Nothing, _fcType = pType_}

-- | The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
fcOptions :: Lens' FirelensConfiguration (HashMap Text (Text))
fcOptions = lens _fcOptions (\s a -> s {_fcOptions = a}) . _Default . _Map

-- | The log router to use. The valid values are @fluentd@ or @fluentbit@ .
fcType :: Lens' FirelensConfiguration FirelensConfigurationType
fcType = lens _fcType (\s a -> s {_fcType = a})

instance FromJSON FirelensConfiguration where
  parseJSON =
    withObject
      "FirelensConfiguration"
      ( \x ->
          FirelensConfiguration'
            <$> (x .:? "options" .!= mempty) <*> (x .: "type")
      )

instance Hashable FirelensConfiguration

instance NFData FirelensConfiguration

instance ToJSON FirelensConfiguration where
  toJSON FirelensConfiguration' {..} =
    object
      ( catMaybes
          [("options" .=) <$> _fcOptions, Just ("type" .= _fcType)]
      )
