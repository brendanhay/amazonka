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
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinition
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition.
module Network.AWS.Greengrass.GetSubscriptionDefinition
    (
    -- * Creating a Request
      getSubscriptionDefinition
    , GetSubscriptionDefinition
    -- * Request Lenses
    , gsdSubscriptionDefinitionId

    -- * Destructuring the Response
    , getSubscriptionDefinitionResponse
    , GetSubscriptionDefinitionResponse
    -- * Response Lenses
    , gsdrsLatestVersionARN
    , gsdrsARN
    , gsdrsName
    , gsdrsCreationTimestamp
    , gsdrsId
    , gsdrsLatestVersion
    , gsdrsLastUpdatedTimestamp
    , gsdrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getSubscriptionDefinition' smart constructor.
newtype GetSubscriptionDefinition = GetSubscriptionDefinition'
    { _gsdSubscriptionDefinitionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdSubscriptionDefinitionId' - subscription definition Id
getSubscriptionDefinition
    :: Text -- ^ 'gsdSubscriptionDefinitionId'
    -> GetSubscriptionDefinition
getSubscriptionDefinition pSubscriptionDefinitionId_ =
    GetSubscriptionDefinition'
    { _gsdSubscriptionDefinitionId = pSubscriptionDefinitionId_
    }

-- | subscription definition Id
gsdSubscriptionDefinitionId :: Lens' GetSubscriptionDefinition Text
gsdSubscriptionDefinitionId = lens _gsdSubscriptionDefinitionId (\ s a -> s{_gsdSubscriptionDefinitionId = a});

instance AWSRequest GetSubscriptionDefinition where
        type Rs GetSubscriptionDefinition =
             GetSubscriptionDefinitionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetSubscriptionDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetSubscriptionDefinition

instance NFData GetSubscriptionDefinition

instance ToHeaders GetSubscriptionDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSubscriptionDefinition where
        toPath GetSubscriptionDefinition'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _gsdSubscriptionDefinitionId]

instance ToQuery GetSubscriptionDefinition where
        toQuery = const mempty

-- | /See:/ 'getSubscriptionDefinitionResponse' smart constructor.
data GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse'
    { _gsdrsLatestVersionARN     :: !(Maybe Text)
    , _gsdrsARN                  :: !(Maybe Text)
    , _gsdrsName                 :: !(Maybe Text)
    , _gsdrsCreationTimestamp    :: !(Maybe Text)
    , _gsdrsId                   :: !(Maybe Text)
    , _gsdrsLatestVersion        :: !(Maybe Text)
    , _gsdrsLastUpdatedTimestamp :: !(Maybe Text)
    , _gsdrsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdrsLatestVersionARN' - Latest version arn of the definition.
--
-- * 'gsdrsARN' - Arn of the definition.
--
-- * 'gsdrsName' - Name of the definition.
--
-- * 'gsdrsCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'gsdrsId' - Id of the definition.
--
-- * 'gsdrsLatestVersion' - Last version of the definition.
--
-- * 'gsdrsLastUpdatedTimestamp' - Last updated timestamp of the definition.
--
-- * 'gsdrsResponseStatus' - -- | The response status code.
getSubscriptionDefinitionResponse
    :: Int -- ^ 'gsdrsResponseStatus'
    -> GetSubscriptionDefinitionResponse
getSubscriptionDefinitionResponse pResponseStatus_ =
    GetSubscriptionDefinitionResponse'
    { _gsdrsLatestVersionARN = Nothing
    , _gsdrsARN = Nothing
    , _gsdrsName = Nothing
    , _gsdrsCreationTimestamp = Nothing
    , _gsdrsId = Nothing
    , _gsdrsLatestVersion = Nothing
    , _gsdrsLastUpdatedTimestamp = Nothing
    , _gsdrsResponseStatus = pResponseStatus_
    }

-- | Latest version arn of the definition.
gsdrsLatestVersionARN :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsLatestVersionARN = lens _gsdrsLatestVersionARN (\ s a -> s{_gsdrsLatestVersionARN = a});

-- | Arn of the definition.
gsdrsARN :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsARN = lens _gsdrsARN (\ s a -> s{_gsdrsARN = a});

-- | Name of the definition.
gsdrsName :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsName = lens _gsdrsName (\ s a -> s{_gsdrsName = a});

-- | Timestamp of when the definition was created.
gsdrsCreationTimestamp :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsCreationTimestamp = lens _gsdrsCreationTimestamp (\ s a -> s{_gsdrsCreationTimestamp = a});

-- | Id of the definition.
gsdrsId :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsId = lens _gsdrsId (\ s a -> s{_gsdrsId = a});

-- | Last version of the definition.
gsdrsLatestVersion :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsLatestVersion = lens _gsdrsLatestVersion (\ s a -> s{_gsdrsLatestVersion = a});

-- | Last updated timestamp of the definition.
gsdrsLastUpdatedTimestamp :: Lens' GetSubscriptionDefinitionResponse (Maybe Text)
gsdrsLastUpdatedTimestamp = lens _gsdrsLastUpdatedTimestamp (\ s a -> s{_gsdrsLastUpdatedTimestamp = a});

-- | -- | The response status code.
gsdrsResponseStatus :: Lens' GetSubscriptionDefinitionResponse Int
gsdrsResponseStatus = lens _gsdrsResponseStatus (\ s a -> s{_gsdrsResponseStatus = a});

instance NFData GetSubscriptionDefinitionResponse
