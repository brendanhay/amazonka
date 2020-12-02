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
-- Module      : Network.AWS.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its creation time and latest version.
module Network.AWS.Greengrass.GetResourceDefinition
    (
    -- * Creating a Request
      getResourceDefinition
    , GetResourceDefinition
    -- * Request Lenses
    , grdResourceDefinitionId

    -- * Destructuring the Response
    , getResourceDefinitionResponse
    , GetResourceDefinitionResponse
    -- * Response Lenses
    , grdrsLatestVersionARN
    , grdrsARN
    , grdrsName
    , grdrsCreationTimestamp
    , grdrsId
    , grdrsLatestVersion
    , grdrsLastUpdatedTimestamp
    , grdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourceDefinition' smart constructor.
newtype GetResourceDefinition = GetResourceDefinition'
  { _grdResourceDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdResourceDefinitionId' - The ID of the resource definition.
getResourceDefinition
    :: Text -- ^ 'grdResourceDefinitionId'
    -> GetResourceDefinition
getResourceDefinition pResourceDefinitionId_ =
  GetResourceDefinition' {_grdResourceDefinitionId = pResourceDefinitionId_}


-- | The ID of the resource definition.
grdResourceDefinitionId :: Lens' GetResourceDefinition Text
grdResourceDefinitionId = lens _grdResourceDefinitionId (\ s a -> s{_grdResourceDefinitionId = a})

instance AWSRequest GetResourceDefinition where
        type Rs GetResourceDefinition =
             GetResourceDefinitionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetResourceDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetResourceDefinition where

instance NFData GetResourceDefinition where

instance ToHeaders GetResourceDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetResourceDefinition where
        toPath GetResourceDefinition'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _grdResourceDefinitionId]

instance ToQuery GetResourceDefinition where
        toQuery = const mempty

-- | /See:/ 'getResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
  { _grdrsLatestVersionARN     :: !(Maybe Text)
  , _grdrsARN                  :: !(Maybe Text)
  , _grdrsName                 :: !(Maybe Text)
  , _grdrsCreationTimestamp    :: !(Maybe Text)
  , _grdrsId                   :: !(Maybe Text)
  , _grdrsLatestVersion        :: !(Maybe Text)
  , _grdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _grdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'grdrsARN' - The ARN of the definition.
--
-- * 'grdrsName' - The name of the definition.
--
-- * 'grdrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'grdrsId' - The ID of the definition.
--
-- * 'grdrsLatestVersion' - The latest version of the definition.
--
-- * 'grdrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'grdrsResponseStatus' - -- | The response status code.
getResourceDefinitionResponse
    :: Int -- ^ 'grdrsResponseStatus'
    -> GetResourceDefinitionResponse
getResourceDefinitionResponse pResponseStatus_ =
  GetResourceDefinitionResponse'
    { _grdrsLatestVersionARN = Nothing
    , _grdrsARN = Nothing
    , _grdrsName = Nothing
    , _grdrsCreationTimestamp = Nothing
    , _grdrsId = Nothing
    , _grdrsLatestVersion = Nothing
    , _grdrsLastUpdatedTimestamp = Nothing
    , _grdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
grdrsLatestVersionARN :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsLatestVersionARN = lens _grdrsLatestVersionARN (\ s a -> s{_grdrsLatestVersionARN = a})

-- | The ARN of the definition.
grdrsARN :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsARN = lens _grdrsARN (\ s a -> s{_grdrsARN = a})

-- | The name of the definition.
grdrsName :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsName = lens _grdrsName (\ s a -> s{_grdrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
grdrsCreationTimestamp :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsCreationTimestamp = lens _grdrsCreationTimestamp (\ s a -> s{_grdrsCreationTimestamp = a})

-- | The ID of the definition.
grdrsId :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsId = lens _grdrsId (\ s a -> s{_grdrsId = a})

-- | The latest version of the definition.
grdrsLatestVersion :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsLatestVersion = lens _grdrsLatestVersion (\ s a -> s{_grdrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
grdrsLastUpdatedTimestamp :: Lens' GetResourceDefinitionResponse (Maybe Text)
grdrsLastUpdatedTimestamp = lens _grdrsLastUpdatedTimestamp (\ s a -> s{_grdrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
grdrsResponseStatus :: Lens' GetResourceDefinitionResponse Int
grdrsResponseStatus = lens _grdrsResponseStatus (\ s a -> s{_grdrsResponseStatus = a})

instance NFData GetResourceDefinitionResponse where
