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
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition version.
module Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
    (
    -- * Creating a Request
      getSubscriptionDefinitionVersion
    , GetSubscriptionDefinitionVersion
    -- * Request Lenses
    , gsdvSubscriptionDefinitionId
    , gsdvSubscriptionDefinitionVersionId

    -- * Destructuring the Response
    , getSubscriptionDefinitionVersionResponse
    , GetSubscriptionDefinitionVersionResponse
    -- * Response Lenses
    , gsdvrsDefinition
    , gsdvrsARN
    , gsdvrsCreationTimestamp
    , gsdvrsVersion
    , gsdvrsId
    , gsdvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getSubscriptionDefinitionVersion' smart constructor.
data GetSubscriptionDefinitionVersion = GetSubscriptionDefinitionVersion'
    { _gsdvSubscriptionDefinitionId        :: !Text
    , _gsdvSubscriptionDefinitionVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdvSubscriptionDefinitionId' - subscription definition Id
--
-- * 'gsdvSubscriptionDefinitionVersionId' - subscription definition version Id
getSubscriptionDefinitionVersion
    :: Text -- ^ 'gsdvSubscriptionDefinitionId'
    -> Text -- ^ 'gsdvSubscriptionDefinitionVersionId'
    -> GetSubscriptionDefinitionVersion
getSubscriptionDefinitionVersion pSubscriptionDefinitionId_ pSubscriptionDefinitionVersionId_ =
    GetSubscriptionDefinitionVersion'
    { _gsdvSubscriptionDefinitionId = pSubscriptionDefinitionId_
    , _gsdvSubscriptionDefinitionVersionId = pSubscriptionDefinitionVersionId_
    }

-- | subscription definition Id
gsdvSubscriptionDefinitionId :: Lens' GetSubscriptionDefinitionVersion Text
gsdvSubscriptionDefinitionId = lens _gsdvSubscriptionDefinitionId (\ s a -> s{_gsdvSubscriptionDefinitionId = a});

-- | subscription definition version Id
gsdvSubscriptionDefinitionVersionId :: Lens' GetSubscriptionDefinitionVersion Text
gsdvSubscriptionDefinitionVersionId = lens _gsdvSubscriptionDefinitionVersionId (\ s a -> s{_gsdvSubscriptionDefinitionVersionId = a});

instance AWSRequest GetSubscriptionDefinitionVersion
         where
        type Rs GetSubscriptionDefinitionVersion =
             GetSubscriptionDefinitionVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetSubscriptionDefinitionVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetSubscriptionDefinitionVersion

instance NFData GetSubscriptionDefinitionVersion

instance ToHeaders GetSubscriptionDefinitionVersion
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSubscriptionDefinitionVersion
         where
        toPath GetSubscriptionDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _gsdvSubscriptionDefinitionId, "/versions/",
               toBS _gsdvSubscriptionDefinitionVersionId]

instance ToQuery GetSubscriptionDefinitionVersion
         where
        toQuery = const mempty

-- | /See:/ 'getSubscriptionDefinitionVersionResponse' smart constructor.
data GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse'
    { _gsdvrsDefinition        :: !(Maybe SubscriptionDefinitionVersion)
    , _gsdvrsARN               :: !(Maybe Text)
    , _gsdvrsCreationTimestamp :: !(Maybe Text)
    , _gsdvrsVersion           :: !(Maybe Text)
    , _gsdvrsId                :: !(Maybe Text)
    , _gsdvrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdvrsDefinition' - Information on the definition
--
-- * 'gsdvrsARN' - Arn of the subscription definition version.
--
-- * 'gsdvrsCreationTimestamp' - Timestamp of when the subscription definition version was created.
--
-- * 'gsdvrsVersion' - Version of the subscription definition version.
--
-- * 'gsdvrsId' - Id of the subscription definition the version belongs to.
--
-- * 'gsdvrsResponseStatus' - -- | The response status code.
getSubscriptionDefinitionVersionResponse
    :: Int -- ^ 'gsdvrsResponseStatus'
    -> GetSubscriptionDefinitionVersionResponse
getSubscriptionDefinitionVersionResponse pResponseStatus_ =
    GetSubscriptionDefinitionVersionResponse'
    { _gsdvrsDefinition = Nothing
    , _gsdvrsARN = Nothing
    , _gsdvrsCreationTimestamp = Nothing
    , _gsdvrsVersion = Nothing
    , _gsdvrsId = Nothing
    , _gsdvrsResponseStatus = pResponseStatus_
    }

-- | Information on the definition
gsdvrsDefinition :: Lens' GetSubscriptionDefinitionVersionResponse (Maybe SubscriptionDefinitionVersion)
gsdvrsDefinition = lens _gsdvrsDefinition (\ s a -> s{_gsdvrsDefinition = a});

-- | Arn of the subscription definition version.
gsdvrsARN :: Lens' GetSubscriptionDefinitionVersionResponse (Maybe Text)
gsdvrsARN = lens _gsdvrsARN (\ s a -> s{_gsdvrsARN = a});

-- | Timestamp of when the subscription definition version was created.
gsdvrsCreationTimestamp :: Lens' GetSubscriptionDefinitionVersionResponse (Maybe Text)
gsdvrsCreationTimestamp = lens _gsdvrsCreationTimestamp (\ s a -> s{_gsdvrsCreationTimestamp = a});

-- | Version of the subscription definition version.
gsdvrsVersion :: Lens' GetSubscriptionDefinitionVersionResponse (Maybe Text)
gsdvrsVersion = lens _gsdvrsVersion (\ s a -> s{_gsdvrsVersion = a});

-- | Id of the subscription definition the version belongs to.
gsdvrsId :: Lens' GetSubscriptionDefinitionVersionResponse (Maybe Text)
gsdvrsId = lens _gsdvrsId (\ s a -> s{_gsdvrsId = a});

-- | -- | The response status code.
gsdvrsResponseStatus :: Lens' GetSubscriptionDefinitionVersionResponse Int
gsdvrsResponseStatus = lens _gsdvrsResponseStatus (\ s a -> s{_gsdvrsResponseStatus = a});

instance NFData
         GetSubscriptionDefinitionVersionResponse
