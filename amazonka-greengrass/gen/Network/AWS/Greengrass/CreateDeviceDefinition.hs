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
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the device definition now or use ''CreateDeviceDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateDeviceDefinition
    (
    -- * Creating a Request
      createDeviceDefinition
    , CreateDeviceDefinition
    -- * Request Lenses
    , cddAmznClientToken
    , cddInitialVersion
    , cddName

    -- * Destructuring the Response
    , createDeviceDefinitionResponse
    , CreateDeviceDefinitionResponse
    -- * Response Lenses
    , cddrsLatestVersionARN
    , cddrsARN
    , cddrsName
    , cddrsCreationTimestamp
    , cddrsId
    , cddrsLatestVersion
    , cddrsLastUpdatedTimestamp
    , cddrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { _cddAmznClientToken :: !(Maybe Text)
  , _cddInitialVersion  :: !(Maybe DeviceDefinitionVersion)
  , _cddName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeviceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cddInitialVersion' - Information about the initial version of the device definition.
--
-- * 'cddName' - The name of the device definition.
createDeviceDefinition
    :: CreateDeviceDefinition
createDeviceDefinition =
  CreateDeviceDefinition'
    { _cddAmznClientToken = Nothing
    , _cddInitialVersion = Nothing
    , _cddName = Nothing
    }


-- | A client token used to correlate requests and responses.
cddAmznClientToken :: Lens' CreateDeviceDefinition (Maybe Text)
cddAmznClientToken = lens _cddAmznClientToken (\ s a -> s{_cddAmznClientToken = a})

-- | Information about the initial version of the device definition.
cddInitialVersion :: Lens' CreateDeviceDefinition (Maybe DeviceDefinitionVersion)
cddInitialVersion = lens _cddInitialVersion (\ s a -> s{_cddInitialVersion = a})

-- | The name of the device definition.
cddName :: Lens' CreateDeviceDefinition (Maybe Text)
cddName = lens _cddName (\ s a -> s{_cddName = a})

instance AWSRequest CreateDeviceDefinition where
        type Rs CreateDeviceDefinition =
             CreateDeviceDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeviceDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDeviceDefinition where

instance NFData CreateDeviceDefinition where

instance ToHeaders CreateDeviceDefinition where
        toHeaders CreateDeviceDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cddAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateDeviceDefinition where
        toJSON CreateDeviceDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _cddInitialVersion,
                  ("Name" .=) <$> _cddName])

instance ToPath CreateDeviceDefinition where
        toPath = const "/greengrass/definition/devices"

instance ToQuery CreateDeviceDefinition where
        toQuery = const mempty

-- | /See:/ 'createDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
  { _cddrsLatestVersionARN     :: !(Maybe Text)
  , _cddrsARN                  :: !(Maybe Text)
  , _cddrsName                 :: !(Maybe Text)
  , _cddrsCreationTimestamp    :: !(Maybe Text)
  , _cddrsId                   :: !(Maybe Text)
  , _cddrsLatestVersion        :: !(Maybe Text)
  , _cddrsLastUpdatedTimestamp :: !(Maybe Text)
  , _cddrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'cddrsARN' - The ARN of the definition.
--
-- * 'cddrsName' - The name of the definition.
--
-- * 'cddrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'cddrsId' - The ID of the definition.
--
-- * 'cddrsLatestVersion' - The latest version of the definition.
--
-- * 'cddrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'cddrsResponseStatus' - -- | The response status code.
createDeviceDefinitionResponse
    :: Int -- ^ 'cddrsResponseStatus'
    -> CreateDeviceDefinitionResponse
createDeviceDefinitionResponse pResponseStatus_ =
  CreateDeviceDefinitionResponse'
    { _cddrsLatestVersionARN = Nothing
    , _cddrsARN = Nothing
    , _cddrsName = Nothing
    , _cddrsCreationTimestamp = Nothing
    , _cddrsId = Nothing
    , _cddrsLatestVersion = Nothing
    , _cddrsLastUpdatedTimestamp = Nothing
    , _cddrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
cddrsLatestVersionARN :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsLatestVersionARN = lens _cddrsLatestVersionARN (\ s a -> s{_cddrsLatestVersionARN = a})

-- | The ARN of the definition.
cddrsARN :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsARN = lens _cddrsARN (\ s a -> s{_cddrsARN = a})

-- | The name of the definition.
cddrsName :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsName = lens _cddrsName (\ s a -> s{_cddrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
cddrsCreationTimestamp :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsCreationTimestamp = lens _cddrsCreationTimestamp (\ s a -> s{_cddrsCreationTimestamp = a})

-- | The ID of the definition.
cddrsId :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsId = lens _cddrsId (\ s a -> s{_cddrsId = a})

-- | The latest version of the definition.
cddrsLatestVersion :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsLatestVersion = lens _cddrsLatestVersion (\ s a -> s{_cddrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
cddrsLastUpdatedTimestamp :: Lens' CreateDeviceDefinitionResponse (Maybe Text)
cddrsLastUpdatedTimestamp = lens _cddrsLastUpdatedTimestamp (\ s a -> s{_cddrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
cddrsResponseStatus :: Lens' CreateDeviceDefinitionResponse Int
cddrsResponseStatus = lens _cddrsResponseStatus (\ s a -> s{_cddrsResponseStatus = a})

instance NFData CreateDeviceDefinitionResponse where
