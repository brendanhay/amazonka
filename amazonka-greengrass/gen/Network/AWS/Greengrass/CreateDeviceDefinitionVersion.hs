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
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a device definition that has already been defined.
module Network.AWS.Greengrass.CreateDeviceDefinitionVersion
    (
    -- * Creating a Request
      createDeviceDefinitionVersion
    , CreateDeviceDefinitionVersion
    -- * Request Lenses
    , cddvAmznClientToken
    , cddvDevices
    , cddvDeviceDefinitionId

    -- * Destructuring the Response
    , createDeviceDefinitionVersionResponse
    , CreateDeviceDefinitionVersionResponse
    -- * Response Lenses
    , cddvrsARN
    , cddvrsCreationTimestamp
    , cddvrsVersion
    , cddvrsId
    , cddvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeviceDefinitionVersion' smart constructor.
data CreateDeviceDefinitionVersion = CreateDeviceDefinitionVersion'
  { _cddvAmznClientToken    :: !(Maybe Text)
  , _cddvDevices            :: !(Maybe [Device])
  , _cddvDeviceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddvAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cddvDevices' - A list of devices in the definition version.
--
-- * 'cddvDeviceDefinitionId' - The ID of the device definition.
createDeviceDefinitionVersion
    :: Text -- ^ 'cddvDeviceDefinitionId'
    -> CreateDeviceDefinitionVersion
createDeviceDefinitionVersion pDeviceDefinitionId_ =
  CreateDeviceDefinitionVersion'
    { _cddvAmznClientToken = Nothing
    , _cddvDevices = Nothing
    , _cddvDeviceDefinitionId = pDeviceDefinitionId_
    }


-- | A client token used to correlate requests and responses.
cddvAmznClientToken :: Lens' CreateDeviceDefinitionVersion (Maybe Text)
cddvAmznClientToken = lens _cddvAmznClientToken (\ s a -> s{_cddvAmznClientToken = a})

-- | A list of devices in the definition version.
cddvDevices :: Lens' CreateDeviceDefinitionVersion [Device]
cddvDevices = lens _cddvDevices (\ s a -> s{_cddvDevices = a}) . _Default . _Coerce

-- | The ID of the device definition.
cddvDeviceDefinitionId :: Lens' CreateDeviceDefinitionVersion Text
cddvDeviceDefinitionId = lens _cddvDeviceDefinitionId (\ s a -> s{_cddvDeviceDefinitionId = a})

instance AWSRequest CreateDeviceDefinitionVersion
         where
        type Rs CreateDeviceDefinitionVersion =
             CreateDeviceDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeviceDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDeviceDefinitionVersion where

instance NFData CreateDeviceDefinitionVersion where

instance ToHeaders CreateDeviceDefinitionVersion
         where
        toHeaders CreateDeviceDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cddvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateDeviceDefinitionVersion where
        toJSON CreateDeviceDefinitionVersion'{..}
          = object
              (catMaybes [("Devices" .=) <$> _cddvDevices])

instance ToPath CreateDeviceDefinitionVersion where
        toPath CreateDeviceDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/devices/",
               toBS _cddvDeviceDefinitionId, "/versions"]

instance ToQuery CreateDeviceDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'createDeviceDefinitionVersionResponse' smart constructor.
data CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse'
  { _cddvrsARN               :: !(Maybe Text)
  , _cddvrsCreationTimestamp :: !(Maybe Text)
  , _cddvrsVersion           :: !(Maybe Text)
  , _cddvrsId                :: !(Maybe Text)
  , _cddvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeviceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddvrsARN' - The ARN of the version.
--
-- * 'cddvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'cddvrsVersion' - The unique ID of the version.
--
-- * 'cddvrsId' - The ID of the version.
--
-- * 'cddvrsResponseStatus' - -- | The response status code.
createDeviceDefinitionVersionResponse
    :: Int -- ^ 'cddvrsResponseStatus'
    -> CreateDeviceDefinitionVersionResponse
createDeviceDefinitionVersionResponse pResponseStatus_ =
  CreateDeviceDefinitionVersionResponse'
    { _cddvrsARN = Nothing
    , _cddvrsCreationTimestamp = Nothing
    , _cddvrsVersion = Nothing
    , _cddvrsId = Nothing
    , _cddvrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the version.
cddvrsARN :: Lens' CreateDeviceDefinitionVersionResponse (Maybe Text)
cddvrsARN = lens _cddvrsARN (\ s a -> s{_cddvrsARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
cddvrsCreationTimestamp :: Lens' CreateDeviceDefinitionVersionResponse (Maybe Text)
cddvrsCreationTimestamp = lens _cddvrsCreationTimestamp (\ s a -> s{_cddvrsCreationTimestamp = a})

-- | The unique ID of the version.
cddvrsVersion :: Lens' CreateDeviceDefinitionVersionResponse (Maybe Text)
cddvrsVersion = lens _cddvrsVersion (\ s a -> s{_cddvrsVersion = a})

-- | The ID of the version.
cddvrsId :: Lens' CreateDeviceDefinitionVersionResponse (Maybe Text)
cddvrsId = lens _cddvrsId (\ s a -> s{_cddvrsId = a})

-- | -- | The response status code.
cddvrsResponseStatus :: Lens' CreateDeviceDefinitionVersionResponse Int
cddvrsResponseStatus = lens _cddvrsResponseStatus (\ s a -> s{_cddvrsResponseStatus = a})

instance NFData CreateDeviceDefinitionVersionResponse
         where
