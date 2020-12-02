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
-- Module      : Network.AWS.Greengrass.GetDeviceDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition version.
module Network.AWS.Greengrass.GetDeviceDefinitionVersion
    (
    -- * Creating a Request
      getDeviceDefinitionVersion
    , GetDeviceDefinitionVersion
    -- * Request Lenses
    , gddvDeviceDefinitionVersionId
    , gddvDeviceDefinitionId

    -- * Destructuring the Response
    , getDeviceDefinitionVersionResponse
    , GetDeviceDefinitionVersionResponse
    -- * Response Lenses
    , gddvrsDefinition
    , gddvrsARN
    , gddvrsCreationTimestamp
    , gddvrsVersion
    , gddvrsId
    , gddvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeviceDefinitionVersion' smart constructor.
data GetDeviceDefinitionVersion = GetDeviceDefinitionVersion'
  { _gddvDeviceDefinitionVersionId :: !Text
  , _gddvDeviceDefinitionId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddvDeviceDefinitionVersionId' - The ID of the device definition version.
--
-- * 'gddvDeviceDefinitionId' - The ID of the device definition.
getDeviceDefinitionVersion
    :: Text -- ^ 'gddvDeviceDefinitionVersionId'
    -> Text -- ^ 'gddvDeviceDefinitionId'
    -> GetDeviceDefinitionVersion
getDeviceDefinitionVersion pDeviceDefinitionVersionId_ pDeviceDefinitionId_ =
  GetDeviceDefinitionVersion'
    { _gddvDeviceDefinitionVersionId = pDeviceDefinitionVersionId_
    , _gddvDeviceDefinitionId = pDeviceDefinitionId_
    }


-- | The ID of the device definition version.
gddvDeviceDefinitionVersionId :: Lens' GetDeviceDefinitionVersion Text
gddvDeviceDefinitionVersionId = lens _gddvDeviceDefinitionVersionId (\ s a -> s{_gddvDeviceDefinitionVersionId = a})

-- | The ID of the device definition.
gddvDeviceDefinitionId :: Lens' GetDeviceDefinitionVersion Text
gddvDeviceDefinitionId = lens _gddvDeviceDefinitionId (\ s a -> s{_gddvDeviceDefinitionId = a})

instance AWSRequest GetDeviceDefinitionVersion where
        type Rs GetDeviceDefinitionVersion =
             GetDeviceDefinitionVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceDefinitionVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeviceDefinitionVersion where

instance NFData GetDeviceDefinitionVersion where

instance ToHeaders GetDeviceDefinitionVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDeviceDefinitionVersion where
        toPath GetDeviceDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/devices/",
               toBS _gddvDeviceDefinitionId, "/versions/",
               toBS _gddvDeviceDefinitionVersionId]

instance ToQuery GetDeviceDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'getDeviceDefinitionVersionResponse' smart constructor.
data GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse'
  { _gddvrsDefinition        :: !(Maybe DeviceDefinitionVersion)
  , _gddvrsARN               :: !(Maybe Text)
  , _gddvrsCreationTimestamp :: !(Maybe Text)
  , _gddvrsVersion           :: !(Maybe Text)
  , _gddvrsId                :: !(Maybe Text)
  , _gddvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddvrsDefinition' - Information about the device definition version.
--
-- * 'gddvrsARN' - The ARN of the device definition version.
--
-- * 'gddvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the device definition version was created.
--
-- * 'gddvrsVersion' - The version of the device definition version.
--
-- * 'gddvrsId' - The ID of the device definition version.
--
-- * 'gddvrsResponseStatus' - -- | The response status code.
getDeviceDefinitionVersionResponse
    :: Int -- ^ 'gddvrsResponseStatus'
    -> GetDeviceDefinitionVersionResponse
getDeviceDefinitionVersionResponse pResponseStatus_ =
  GetDeviceDefinitionVersionResponse'
    { _gddvrsDefinition = Nothing
    , _gddvrsARN = Nothing
    , _gddvrsCreationTimestamp = Nothing
    , _gddvrsVersion = Nothing
    , _gddvrsId = Nothing
    , _gddvrsResponseStatus = pResponseStatus_
    }


-- | Information about the device definition version.
gddvrsDefinition :: Lens' GetDeviceDefinitionVersionResponse (Maybe DeviceDefinitionVersion)
gddvrsDefinition = lens _gddvrsDefinition (\ s a -> s{_gddvrsDefinition = a})

-- | The ARN of the device definition version.
gddvrsARN :: Lens' GetDeviceDefinitionVersionResponse (Maybe Text)
gddvrsARN = lens _gddvrsARN (\ s a -> s{_gddvrsARN = a})

-- | The time, in milliseconds since the epoch, when the device definition version was created.
gddvrsCreationTimestamp :: Lens' GetDeviceDefinitionVersionResponse (Maybe Text)
gddvrsCreationTimestamp = lens _gddvrsCreationTimestamp (\ s a -> s{_gddvrsCreationTimestamp = a})

-- | The version of the device definition version.
gddvrsVersion :: Lens' GetDeviceDefinitionVersionResponse (Maybe Text)
gddvrsVersion = lens _gddvrsVersion (\ s a -> s{_gddvrsVersion = a})

-- | The ID of the device definition version.
gddvrsId :: Lens' GetDeviceDefinitionVersionResponse (Maybe Text)
gddvrsId = lens _gddvrsId (\ s a -> s{_gddvrsId = a})

-- | -- | The response status code.
gddvrsResponseStatus :: Lens' GetDeviceDefinitionVersionResponse Int
gddvrsResponseStatus = lens _gddvrsResponseStatus (\ s a -> s{_gddvrsResponseStatus = a})

instance NFData GetDeviceDefinitionVersionResponse
         where
