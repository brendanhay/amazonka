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
-- Module      : Network.AWS.Greengrass.GetDeviceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition.
module Network.AWS.Greengrass.GetDeviceDefinition
    (
    -- * Creating a Request
      getDeviceDefinition
    , GetDeviceDefinition
    -- * Request Lenses
    , gddDeviceDefinitionId

    -- * Destructuring the Response
    , getDeviceDefinitionResponse
    , GetDeviceDefinitionResponse
    -- * Response Lenses
    , gddrsLatestVersionARN
    , gddrsARN
    , gddrsName
    , gddrsCreationTimestamp
    , gddrsId
    , gddrsLatestVersion
    , gddrsLastUpdatedTimestamp
    , gddrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeviceDefinition' smart constructor.
newtype GetDeviceDefinition = GetDeviceDefinition'
  { _gddDeviceDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddDeviceDefinitionId' - The ID of the device definition.
getDeviceDefinition
    :: Text -- ^ 'gddDeviceDefinitionId'
    -> GetDeviceDefinition
getDeviceDefinition pDeviceDefinitionId_ =
  GetDeviceDefinition' {_gddDeviceDefinitionId = pDeviceDefinitionId_}


-- | The ID of the device definition.
gddDeviceDefinitionId :: Lens' GetDeviceDefinition Text
gddDeviceDefinitionId = lens _gddDeviceDefinitionId (\ s a -> s{_gddDeviceDefinitionId = a})

instance AWSRequest GetDeviceDefinition where
        type Rs GetDeviceDefinition =
             GetDeviceDefinitionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeviceDefinition where

instance NFData GetDeviceDefinition where

instance ToHeaders GetDeviceDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDeviceDefinition where
        toPath GetDeviceDefinition'{..}
          = mconcat
              ["/greengrass/definition/devices/",
               toBS _gddDeviceDefinitionId]

instance ToQuery GetDeviceDefinition where
        toQuery = const mempty

-- | /See:/ 'getDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
  { _gddrsLatestVersionARN     :: !(Maybe Text)
  , _gddrsARN                  :: !(Maybe Text)
  , _gddrsName                 :: !(Maybe Text)
  , _gddrsCreationTimestamp    :: !(Maybe Text)
  , _gddrsId                   :: !(Maybe Text)
  , _gddrsLatestVersion        :: !(Maybe Text)
  , _gddrsLastUpdatedTimestamp :: !(Maybe Text)
  , _gddrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'gddrsARN' - The ARN of the definition.
--
-- * 'gddrsName' - The name of the definition.
--
-- * 'gddrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'gddrsId' - The ID of the definition.
--
-- * 'gddrsLatestVersion' - The latest version of the definition.
--
-- * 'gddrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'gddrsResponseStatus' - -- | The response status code.
getDeviceDefinitionResponse
    :: Int -- ^ 'gddrsResponseStatus'
    -> GetDeviceDefinitionResponse
getDeviceDefinitionResponse pResponseStatus_ =
  GetDeviceDefinitionResponse'
    { _gddrsLatestVersionARN = Nothing
    , _gddrsARN = Nothing
    , _gddrsName = Nothing
    , _gddrsCreationTimestamp = Nothing
    , _gddrsId = Nothing
    , _gddrsLatestVersion = Nothing
    , _gddrsLastUpdatedTimestamp = Nothing
    , _gddrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
gddrsLatestVersionARN :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsLatestVersionARN = lens _gddrsLatestVersionARN (\ s a -> s{_gddrsLatestVersionARN = a})

-- | The ARN of the definition.
gddrsARN :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsARN = lens _gddrsARN (\ s a -> s{_gddrsARN = a})

-- | The name of the definition.
gddrsName :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsName = lens _gddrsName (\ s a -> s{_gddrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
gddrsCreationTimestamp :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsCreationTimestamp = lens _gddrsCreationTimestamp (\ s a -> s{_gddrsCreationTimestamp = a})

-- | The ID of the definition.
gddrsId :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsId = lens _gddrsId (\ s a -> s{_gddrsId = a})

-- | The latest version of the definition.
gddrsLatestVersion :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsLatestVersion = lens _gddrsLatestVersion (\ s a -> s{_gddrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
gddrsLastUpdatedTimestamp :: Lens' GetDeviceDefinitionResponse (Maybe Text)
gddrsLastUpdatedTimestamp = lens _gddrsLastUpdatedTimestamp (\ s a -> s{_gddrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
gddrsResponseStatus :: Lens' GetDeviceDefinitionResponse Int
gddrsResponseStatus = lens _gddrsResponseStatus (\ s a -> s{_gddrsResponseStatus = a})

instance NFData GetDeviceDefinitionResponse where
