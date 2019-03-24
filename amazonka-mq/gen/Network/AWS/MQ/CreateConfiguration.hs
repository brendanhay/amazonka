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
-- Module      : Network.AWS.MQ.CreateConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
module Network.AWS.MQ.CreateConfiguration
    (
    -- * Creating a Request
      createConfiguration
    , CreateConfiguration
    -- * Request Lenses
    , ccEngineVersion
    , ccName
    , ccEngineType
    , ccTags

    -- * Destructuring the Response
    , createConfigurationResponse
    , CreateConfigurationResponse
    -- * Response Lenses
    , ccrsARN
    , ccrsLatestRevision
    , ccrsCreated
    , ccrsName
    , ccrsId
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'createConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { _ccEngineVersion :: !(Maybe Text)
  , _ccName          :: !(Maybe Text)
  , _ccEngineType    :: !(Maybe EngineType)
  , _ccTags          :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccEngineVersion' - Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- * 'ccName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'ccEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
--
-- * 'ccTags' - Create tags when creating the configuration.
createConfiguration
    :: CreateConfiguration
createConfiguration =
  CreateConfiguration'
    { _ccEngineVersion = Nothing
    , _ccName = Nothing
    , _ccEngineType = Nothing
    , _ccTags = Nothing
    }


-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
ccEngineVersion :: Lens' CreateConfiguration (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\ s a -> s{_ccEngineVersion = a})

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
ccName :: Lens' CreateConfiguration (Maybe Text)
ccName = lens _ccName (\ s a -> s{_ccName = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
ccEngineType :: Lens' CreateConfiguration (Maybe EngineType)
ccEngineType = lens _ccEngineType (\ s a -> s{_ccEngineType = a})

-- | Create tags when creating the configuration.
ccTags :: Lens' CreateConfiguration (HashMap Text Text)
ccTags = lens _ccTags (\ s a -> s{_ccTags = a}) . _Default . _Map

instance AWSRequest CreateConfiguration where
        type Rs CreateConfiguration =
             CreateConfigurationResponse
        request = postJSON mq
        response
          = receiveJSON
              (\ s h x ->
                 CreateConfigurationResponse' <$>
                   (x .?> "arn") <*> (x .?> "latestRevision") <*>
                     (x .?> "created")
                     <*> (x .?> "name")
                     <*> (x .?> "id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateConfiguration where

instance NFData CreateConfiguration where

instance ToHeaders CreateConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConfiguration where
        toJSON CreateConfiguration'{..}
          = object
              (catMaybes
                 [("engineVersion" .=) <$> _ccEngineVersion,
                  ("name" .=) <$> _ccName,
                  ("engineType" .=) <$> _ccEngineType,
                  ("tags" .=) <$> _ccTags])

instance ToPath CreateConfiguration where
        toPath = const "/v1/configurations"

instance ToQuery CreateConfiguration where
        toQuery = const mempty

-- | /See:/ 'createConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { _ccrsARN            :: !(Maybe Text)
  , _ccrsLatestRevision :: !(Maybe ConfigurationRevision)
  , _ccrsCreated        :: !(Maybe POSIX)
  , _ccrsName           :: !(Maybe Text)
  , _ccrsId             :: !(Maybe Text)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsARN' - Required. The Amazon Resource Name (ARN) of the configuration.
--
-- * 'ccrsLatestRevision' - The latest revision of the configuration.
--
-- * 'ccrsCreated' - Required. The date and time of the configuration.
--
-- * 'ccrsName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'ccrsId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createConfigurationResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateConfigurationResponse
createConfigurationResponse pResponseStatus_ =
  CreateConfigurationResponse'
    { _ccrsARN = Nothing
    , _ccrsLatestRevision = Nothing
    , _ccrsCreated = Nothing
    , _ccrsName = Nothing
    , _ccrsId = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | Required. The Amazon Resource Name (ARN) of the configuration.
ccrsARN :: Lens' CreateConfigurationResponse (Maybe Text)
ccrsARN = lens _ccrsARN (\ s a -> s{_ccrsARN = a})

-- | The latest revision of the configuration.
ccrsLatestRevision :: Lens' CreateConfigurationResponse (Maybe ConfigurationRevision)
ccrsLatestRevision = lens _ccrsLatestRevision (\ s a -> s{_ccrsLatestRevision = a})

-- | Required. The date and time of the configuration.
ccrsCreated :: Lens' CreateConfigurationResponse (Maybe UTCTime)
ccrsCreated = lens _ccrsCreated (\ s a -> s{_ccrsCreated = a}) . mapping _Time

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
ccrsName :: Lens' CreateConfigurationResponse (Maybe Text)
ccrsName = lens _ccrsName (\ s a -> s{_ccrsName = a})

-- | Required. The unique ID that Amazon MQ generates for the configuration.
ccrsId :: Lens' CreateConfigurationResponse (Maybe Text)
ccrsId = lens _ccrsId (\ s a -> s{_ccrsId = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateConfigurationResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateConfigurationResponse where
