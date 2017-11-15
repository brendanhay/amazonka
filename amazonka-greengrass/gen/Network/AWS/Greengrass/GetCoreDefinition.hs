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
-- Module      : Network.AWS.Greengrass.GetCoreDefinition
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinition
    (
    -- * Creating a Request
      getCoreDefinition
    , GetCoreDefinition
    -- * Request Lenses
    , gcdCoreDefinitionId

    -- * Destructuring the Response
    , getCoreDefinitionResponse
    , GetCoreDefinitionResponse
    -- * Response Lenses
    , gcdrsLatestVersionARN
    , gcdrsARN
    , gcdrsName
    , gcdrsCreationTimestamp
    , gcdrsId
    , gcdrsLatestVersion
    , gcdrsLastUpdatedTimestamp
    , gcdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCoreDefinition' smart constructor.
newtype GetCoreDefinition = GetCoreDefinition'
  { _gcdCoreDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCoreDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdCoreDefinitionId' - core definition Id
getCoreDefinition
    :: Text -- ^ 'gcdCoreDefinitionId'
    -> GetCoreDefinition
getCoreDefinition pCoreDefinitionId_ =
  GetCoreDefinition' {_gcdCoreDefinitionId = pCoreDefinitionId_}


-- | core definition Id
gcdCoreDefinitionId :: Lens' GetCoreDefinition Text
gcdCoreDefinitionId = lens _gcdCoreDefinitionId (\ s a -> s{_gcdCoreDefinitionId = a});

instance AWSRequest GetCoreDefinition where
        type Rs GetCoreDefinition = GetCoreDefinitionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetCoreDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetCoreDefinition where

instance NFData GetCoreDefinition where

instance ToHeaders GetCoreDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCoreDefinition where
        toPath GetCoreDefinition'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _gcdCoreDefinitionId]

instance ToQuery GetCoreDefinition where
        toQuery = const mempty

-- | /See:/ 'getCoreDefinitionResponse' smart constructor.
data GetCoreDefinitionResponse = GetCoreDefinitionResponse'
  { _gcdrsLatestVersionARN     :: !(Maybe Text)
  , _gcdrsARN                  :: !(Maybe Text)
  , _gcdrsName                 :: !(Maybe Text)
  , _gcdrsCreationTimestamp    :: !(Maybe Text)
  , _gcdrsId                   :: !(Maybe Text)
  , _gcdrsLatestVersion        :: !(Maybe Text)
  , _gcdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _gcdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdrsLatestVersionARN' - Latest version arn of the definition.
--
-- * 'gcdrsARN' - Arn of the definition.
--
-- * 'gcdrsName' - Name of the definition.
--
-- * 'gcdrsCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'gcdrsId' - Id of the definition.
--
-- * 'gcdrsLatestVersion' - Last version of the definition.
--
-- * 'gcdrsLastUpdatedTimestamp' - Last updated timestamp of the definition.
--
-- * 'gcdrsResponseStatus' - -- | The response status code.
getCoreDefinitionResponse
    :: Int -- ^ 'gcdrsResponseStatus'
    -> GetCoreDefinitionResponse
getCoreDefinitionResponse pResponseStatus_ =
  GetCoreDefinitionResponse'
  { _gcdrsLatestVersionARN = Nothing
  , _gcdrsARN = Nothing
  , _gcdrsName = Nothing
  , _gcdrsCreationTimestamp = Nothing
  , _gcdrsId = Nothing
  , _gcdrsLatestVersion = Nothing
  , _gcdrsLastUpdatedTimestamp = Nothing
  , _gcdrsResponseStatus = pResponseStatus_
  }


-- | Latest version arn of the definition.
gcdrsLatestVersionARN :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsLatestVersionARN = lens _gcdrsLatestVersionARN (\ s a -> s{_gcdrsLatestVersionARN = a});

-- | Arn of the definition.
gcdrsARN :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsARN = lens _gcdrsARN (\ s a -> s{_gcdrsARN = a});

-- | Name of the definition.
gcdrsName :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsName = lens _gcdrsName (\ s a -> s{_gcdrsName = a});

-- | Timestamp of when the definition was created.
gcdrsCreationTimestamp :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsCreationTimestamp = lens _gcdrsCreationTimestamp (\ s a -> s{_gcdrsCreationTimestamp = a});

-- | Id of the definition.
gcdrsId :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsId = lens _gcdrsId (\ s a -> s{_gcdrsId = a});

-- | Last version of the definition.
gcdrsLatestVersion :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsLatestVersion = lens _gcdrsLatestVersion (\ s a -> s{_gcdrsLatestVersion = a});

-- | Last updated timestamp of the definition.
gcdrsLastUpdatedTimestamp :: Lens' GetCoreDefinitionResponse (Maybe Text)
gcdrsLastUpdatedTimestamp = lens _gcdrsLastUpdatedTimestamp (\ s a -> s{_gcdrsLastUpdatedTimestamp = a});

-- | -- | The response status code.
gcdrsResponseStatus :: Lens' GetCoreDefinitionResponse Int
gcdrsResponseStatus = lens _gcdrsResponseStatus (\ s a -> s{_gcdrsResponseStatus = a});

instance NFData GetCoreDefinitionResponse where
