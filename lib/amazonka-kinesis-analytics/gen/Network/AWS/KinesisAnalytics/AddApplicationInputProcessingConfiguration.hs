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
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an 'InputProcessingConfiguration' to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <https://aws.amazon.com/documentation/lambda/ AWS Lambda> .
--
--
module Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
    (
    -- * Creating a Request
      addApplicationInputProcessingConfiguration
    , AddApplicationInputProcessingConfiguration
    -- * Request Lenses
    , aaipcApplicationName
    , aaipcCurrentApplicationVersionId
    , aaipcInputId
    , aaipcInputProcessingConfiguration

    -- * Destructuring the Response
    , addApplicationInputProcessingConfigurationResponse
    , AddApplicationInputProcessingConfigurationResponse
    -- * Response Lenses
    , aaipcrsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addApplicationInputProcessingConfiguration' smart constructor.
data AddApplicationInputProcessingConfiguration = AddApplicationInputProcessingConfiguration'
  { _aaipcApplicationName              :: !Text
  , _aaipcCurrentApplicationVersionId  :: !Nat
  , _aaipcInputId                      :: !Text
  , _aaipcInputProcessingConfiguration :: !InputProcessingConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationInputProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaipcApplicationName' - Name of the application to which you want to add the input processing configuration.
--
-- * 'aaipcCurrentApplicationVersionId' - Version of the application to which you want to add the input processing configuration. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- * 'aaipcInputId' - The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the 'DescribeApplication' operation.
--
-- * 'aaipcInputProcessingConfiguration' - The 'InputProcessingConfiguration' to add to the application.
addApplicationInputProcessingConfiguration
    :: Text -- ^ 'aaipcApplicationName'
    -> Natural -- ^ 'aaipcCurrentApplicationVersionId'
    -> Text -- ^ 'aaipcInputId'
    -> InputProcessingConfiguration -- ^ 'aaipcInputProcessingConfiguration'
    -> AddApplicationInputProcessingConfiguration
addApplicationInputProcessingConfiguration pApplicationName_ pCurrentApplicationVersionId_ pInputId_ pInputProcessingConfiguration_ =
  AddApplicationInputProcessingConfiguration'
    { _aaipcApplicationName = pApplicationName_
    , _aaipcCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _aaipcInputId = pInputId_
    , _aaipcInputProcessingConfiguration = pInputProcessingConfiguration_
    }


-- | Name of the application to which you want to add the input processing configuration.
aaipcApplicationName :: Lens' AddApplicationInputProcessingConfiguration Text
aaipcApplicationName = lens _aaipcApplicationName (\ s a -> s{_aaipcApplicationName = a})

-- | Version of the application to which you want to add the input processing configuration. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
aaipcCurrentApplicationVersionId :: Lens' AddApplicationInputProcessingConfiguration Natural
aaipcCurrentApplicationVersionId = lens _aaipcCurrentApplicationVersionId (\ s a -> s{_aaipcCurrentApplicationVersionId = a}) . _Nat

-- | The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the 'DescribeApplication' operation.
aaipcInputId :: Lens' AddApplicationInputProcessingConfiguration Text
aaipcInputId = lens _aaipcInputId (\ s a -> s{_aaipcInputId = a})

-- | The 'InputProcessingConfiguration' to add to the application.
aaipcInputProcessingConfiguration :: Lens' AddApplicationInputProcessingConfiguration InputProcessingConfiguration
aaipcInputProcessingConfiguration = lens _aaipcInputProcessingConfiguration (\ s a -> s{_aaipcInputProcessingConfiguration = a})

instance AWSRequest
           AddApplicationInputProcessingConfiguration
         where
        type Rs AddApplicationInputProcessingConfiguration =
             AddApplicationInputProcessingConfigurationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 AddApplicationInputProcessingConfigurationResponse'
                   <$> (pure (fromEnum s)))

instance Hashable
           AddApplicationInputProcessingConfiguration
         where

instance NFData
           AddApplicationInputProcessingConfiguration
         where

instance ToHeaders
           AddApplicationInputProcessingConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.AddApplicationInputProcessingConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           AddApplicationInputProcessingConfiguration
         where
        toJSON
          AddApplicationInputProcessingConfiguration'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _aaipcApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _aaipcCurrentApplicationVersionId),
                  Just ("InputId" .= _aaipcInputId),
                  Just
                    ("InputProcessingConfiguration" .=
                       _aaipcInputProcessingConfiguration)])

instance ToPath
           AddApplicationInputProcessingConfiguration
         where
        toPath = const "/"

instance ToQuery
           AddApplicationInputProcessingConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'addApplicationInputProcessingConfigurationResponse' smart constructor.
newtype AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse'
  { _aaipcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationInputProcessingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaipcrsResponseStatus' - -- | The response status code.
addApplicationInputProcessingConfigurationResponse
    :: Int -- ^ 'aaipcrsResponseStatus'
    -> AddApplicationInputProcessingConfigurationResponse
addApplicationInputProcessingConfigurationResponse pResponseStatus_ =
  AddApplicationInputProcessingConfigurationResponse'
    {_aaipcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aaipcrsResponseStatus :: Lens' AddApplicationInputProcessingConfigurationResponse Int
aaipcrsResponseStatus = lens _aaipcrsResponseStatus (\ s a -> s{_aaipcrsResponseStatus = a})

instance NFData
           AddApplicationInputProcessingConfigurationResponse
         where
