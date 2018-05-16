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
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version of the subscription definition now or use ''CreateSubscriptionDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateSubscriptionDefinition
    (
    -- * Creating a Request
      createSubscriptionDefinition
    , CreateSubscriptionDefinition
    -- * Request Lenses
    , csdAmznClientToken
    , csdInitialVersion
    , csdName

    -- * Destructuring the Response
    , createSubscriptionDefinitionResponse
    , CreateSubscriptionDefinitionResponse
    -- * Response Lenses
    , csdrsLatestVersionARN
    , csdrsARN
    , csdrsName
    , csdrsCreationTimestamp
    , csdrsId
    , csdrsLatestVersion
    , csdrsLastUpdatedTimestamp
    , csdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { _csdAmznClientToken :: !(Maybe Text)
  , _csdInitialVersion  :: !(Maybe SubscriptionDefinitionVersion)
  , _csdName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriptionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'csdInitialVersion' - Information about the initial version of the subscription definition.
--
-- * 'csdName' - The name of the subscription definition.
createSubscriptionDefinition
    :: CreateSubscriptionDefinition
createSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { _csdAmznClientToken = Nothing
    , _csdInitialVersion = Nothing
    , _csdName = Nothing
    }


-- | A client token used to correlate requests and responses.
csdAmznClientToken :: Lens' CreateSubscriptionDefinition (Maybe Text)
csdAmznClientToken = lens _csdAmznClientToken (\ s a -> s{_csdAmznClientToken = a})

-- | Information about the initial version of the subscription definition.
csdInitialVersion :: Lens' CreateSubscriptionDefinition (Maybe SubscriptionDefinitionVersion)
csdInitialVersion = lens _csdInitialVersion (\ s a -> s{_csdInitialVersion = a})

-- | The name of the subscription definition.
csdName :: Lens' CreateSubscriptionDefinition (Maybe Text)
csdName = lens _csdName (\ s a -> s{_csdName = a})

instance AWSRequest CreateSubscriptionDefinition
         where
        type Rs CreateSubscriptionDefinition =
             CreateSubscriptionDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateSubscriptionDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateSubscriptionDefinition where

instance NFData CreateSubscriptionDefinition where

instance ToHeaders CreateSubscriptionDefinition where
        toHeaders CreateSubscriptionDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _csdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateSubscriptionDefinition where
        toJSON CreateSubscriptionDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _csdInitialVersion,
                  ("Name" .=) <$> _csdName])

instance ToPath CreateSubscriptionDefinition where
        toPath = const "/greengrass/definition/subscriptions"

instance ToQuery CreateSubscriptionDefinition where
        toQuery = const mempty

-- | /See:/ 'createSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
  { _csdrsLatestVersionARN     :: !(Maybe Text)
  , _csdrsARN                  :: !(Maybe Text)
  , _csdrsName                 :: !(Maybe Text)
  , _csdrsCreationTimestamp    :: !(Maybe Text)
  , _csdrsId                   :: !(Maybe Text)
  , _csdrsLatestVersion        :: !(Maybe Text)
  , _csdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _csdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'csdrsARN' - The ARN of the definition.
--
-- * 'csdrsName' - The name of the definition.
--
-- * 'csdrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'csdrsId' - The ID of the definition.
--
-- * 'csdrsLatestVersion' - The latest version of the definition.
--
-- * 'csdrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'csdrsResponseStatus' - -- | The response status code.
createSubscriptionDefinitionResponse
    :: Int -- ^ 'csdrsResponseStatus'
    -> CreateSubscriptionDefinitionResponse
createSubscriptionDefinitionResponse pResponseStatus_ =
  CreateSubscriptionDefinitionResponse'
    { _csdrsLatestVersionARN = Nothing
    , _csdrsARN = Nothing
    , _csdrsName = Nothing
    , _csdrsCreationTimestamp = Nothing
    , _csdrsId = Nothing
    , _csdrsLatestVersion = Nothing
    , _csdrsLastUpdatedTimestamp = Nothing
    , _csdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
csdrsLatestVersionARN :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsLatestVersionARN = lens _csdrsLatestVersionARN (\ s a -> s{_csdrsLatestVersionARN = a})

-- | The ARN of the definition.
csdrsARN :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsARN = lens _csdrsARN (\ s a -> s{_csdrsARN = a})

-- | The name of the definition.
csdrsName :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsName = lens _csdrsName (\ s a -> s{_csdrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
csdrsCreationTimestamp :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsCreationTimestamp = lens _csdrsCreationTimestamp (\ s a -> s{_csdrsCreationTimestamp = a})

-- | The ID of the definition.
csdrsId :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsId = lens _csdrsId (\ s a -> s{_csdrsId = a})

-- | The latest version of the definition.
csdrsLatestVersion :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsLatestVersion = lens _csdrsLatestVersion (\ s a -> s{_csdrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
csdrsLastUpdatedTimestamp :: Lens' CreateSubscriptionDefinitionResponse (Maybe Text)
csdrsLastUpdatedTimestamp = lens _csdrsLastUpdatedTimestamp (\ s a -> s{_csdrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
csdrsResponseStatus :: Lens' CreateSubscriptionDefinitionResponse Int
csdrsResponseStatus = lens _csdrsResponseStatus (\ s a -> s{_csdrsResponseStatus = a})

instance NFData CreateSubscriptionDefinitionResponse
         where
