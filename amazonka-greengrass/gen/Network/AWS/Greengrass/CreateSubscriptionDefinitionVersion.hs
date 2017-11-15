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
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a subscription definition which has already been defined.
module Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
    (
    -- * Creating a Request
      createSubscriptionDefinitionVersion
    , CreateSubscriptionDefinitionVersion
    -- * Request Lenses
    , csdvAmznClientToken
    , csdvSubscriptions
    , csdvSubscriptionDefinitionId

    -- * Destructuring the Response
    , createSubscriptionDefinitionVersionResponse
    , CreateSubscriptionDefinitionVersionResponse
    -- * Response Lenses
    , csdvrsARN
    , csdvrsCreationTimestamp
    , csdvrsVersion
    , csdvrsId
    , csdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { _csdvAmznClientToken          :: !(Maybe Text)
  , _csdvSubscriptions            :: !(Maybe [Subscription])
  , _csdvSubscriptionDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdvAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'csdvSubscriptions' - Subscriptions in the version.
--
-- * 'csdvSubscriptionDefinitionId' - subscription definition Id
createSubscriptionDefinitionVersion
    :: Text -- ^ 'csdvSubscriptionDefinitionId'
    -> CreateSubscriptionDefinitionVersion
createSubscriptionDefinitionVersion pSubscriptionDefinitionId_ =
  CreateSubscriptionDefinitionVersion'
  { _csdvAmznClientToken = Nothing
  , _csdvSubscriptions = Nothing
  , _csdvSubscriptionDefinitionId = pSubscriptionDefinitionId_
  }


-- | The client token used to request idempotent operations.
csdvAmznClientToken :: Lens' CreateSubscriptionDefinitionVersion (Maybe Text)
csdvAmznClientToken = lens _csdvAmznClientToken (\ s a -> s{_csdvAmznClientToken = a});

-- | Subscriptions in the version.
csdvSubscriptions :: Lens' CreateSubscriptionDefinitionVersion [Subscription]
csdvSubscriptions = lens _csdvSubscriptions (\ s a -> s{_csdvSubscriptions = a}) . _Default . _Coerce;

-- | subscription definition Id
csdvSubscriptionDefinitionId :: Lens' CreateSubscriptionDefinitionVersion Text
csdvSubscriptionDefinitionId = lens _csdvSubscriptionDefinitionId (\ s a -> s{_csdvSubscriptionDefinitionId = a});

instance AWSRequest
           CreateSubscriptionDefinitionVersion
         where
        type Rs CreateSubscriptionDefinitionVersion =
             CreateSubscriptionDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateSubscriptionDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateSubscriptionDefinitionVersion
         where

instance NFData CreateSubscriptionDefinitionVersion
         where

instance ToHeaders
           CreateSubscriptionDefinitionVersion
         where
        toHeaders CreateSubscriptionDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _csdvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateSubscriptionDefinitionVersion
         where
        toJSON CreateSubscriptionDefinitionVersion'{..}
          = object
              (catMaybes
                 [("Subscriptions" .=) <$> _csdvSubscriptions])

instance ToPath CreateSubscriptionDefinitionVersion
         where
        toPath CreateSubscriptionDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/subscriptions/",
               toBS _csdvSubscriptionDefinitionId, "/versions"]

instance ToQuery CreateSubscriptionDefinitionVersion
         where
        toQuery = const mempty

-- | /See:/ 'createSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
  { _csdvrsARN               :: !(Maybe Text)
  , _csdvrsCreationTimestamp :: !(Maybe Text)
  , _csdvrsVersion           :: !(Maybe Text)
  , _csdvrsId                :: !(Maybe Text)
  , _csdvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriptionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdvrsARN' - Arn of the version.
--
-- * 'csdvrsCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'csdvrsVersion' - Unique Id of a version.
--
-- * 'csdvrsId' - Id of the resource container.
--
-- * 'csdvrsResponseStatus' - -- | The response status code.
createSubscriptionDefinitionVersionResponse
    :: Int -- ^ 'csdvrsResponseStatus'
    -> CreateSubscriptionDefinitionVersionResponse
createSubscriptionDefinitionVersionResponse pResponseStatus_ =
  CreateSubscriptionDefinitionVersionResponse'
  { _csdvrsARN = Nothing
  , _csdvrsCreationTimestamp = Nothing
  , _csdvrsVersion = Nothing
  , _csdvrsId = Nothing
  , _csdvrsResponseStatus = pResponseStatus_
  }


-- | Arn of the version.
csdvrsARN :: Lens' CreateSubscriptionDefinitionVersionResponse (Maybe Text)
csdvrsARN = lens _csdvrsARN (\ s a -> s{_csdvrsARN = a});

-- | Timestamp of when the version was created.
csdvrsCreationTimestamp :: Lens' CreateSubscriptionDefinitionVersionResponse (Maybe Text)
csdvrsCreationTimestamp = lens _csdvrsCreationTimestamp (\ s a -> s{_csdvrsCreationTimestamp = a});

-- | Unique Id of a version.
csdvrsVersion :: Lens' CreateSubscriptionDefinitionVersionResponse (Maybe Text)
csdvrsVersion = lens _csdvrsVersion (\ s a -> s{_csdvrsVersion = a});

-- | Id of the resource container.
csdvrsId :: Lens' CreateSubscriptionDefinitionVersionResponse (Maybe Text)
csdvrsId = lens _csdvrsId (\ s a -> s{_csdvrsId = a});

-- | -- | The response status code.
csdvrsResponseStatus :: Lens' CreateSubscriptionDefinitionVersionResponse Int
csdvrsResponseStatus = lens _csdvrsResponseStatus (\ s a -> s{_csdvrsResponseStatus = a});

instance NFData
           CreateSubscriptionDefinitionVersionResponse
         where
