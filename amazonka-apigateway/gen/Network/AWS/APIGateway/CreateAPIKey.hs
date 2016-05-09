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
-- Module      : Network.AWS.APIGateway.CreateAPIKey
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an < ApiKey> resource.
module Network.AWS.APIGateway.CreateAPIKey
    (
    -- * Creating a Request
      createAPIKey
    , CreateAPIKey
    -- * Request Lenses
    , cakEnabled
    , cakName
    , cakStageKeys
    , cakDescription

    -- * Destructuring the Response
    , apiKey
    , APIKey
    -- * Response Lenses
    , akEnabled
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to create an < ApiKey> resource.
--
-- /See:/ 'createAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
    { _cakEnabled     :: !(Maybe Bool)
    , _cakName        :: !(Maybe Text)
    , _cakStageKeys   :: !(Maybe [StageKey])
    , _cakDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakEnabled'
--
-- * 'cakName'
--
-- * 'cakStageKeys'
--
-- * 'cakDescription'
createAPIKey
    :: CreateAPIKey
createAPIKey =
    CreateAPIKey'
    { _cakEnabled = Nothing
    , _cakName = Nothing
    , _cakStageKeys = Nothing
    , _cakDescription = Nothing
    }

-- | Specifies whether the < ApiKey> can be used by callers.
cakEnabled :: Lens' CreateAPIKey (Maybe Bool)
cakEnabled = lens _cakEnabled (\ s a -> s{_cakEnabled = a});

-- | The name of the < ApiKey>.
cakName :: Lens' CreateAPIKey (Maybe Text)
cakName = lens _cakName (\ s a -> s{_cakName = a});

-- | Specifies whether the < ApiKey> can be used by callers.
cakStageKeys :: Lens' CreateAPIKey [StageKey]
cakStageKeys = lens _cakStageKeys (\ s a -> s{_cakStageKeys = a}) . _Default . _Coerce;

-- | The description of the < ApiKey>.
cakDescription :: Lens' CreateAPIKey (Maybe Text)
cakDescription = lens _cakDescription (\ s a -> s{_cakDescription = a});

instance AWSRequest CreateAPIKey where
        type Rs CreateAPIKey = APIKey
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAPIKey

instance NFData CreateAPIKey

instance ToHeaders CreateAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateAPIKey where
        toJSON CreateAPIKey'{..}
          = object
              (catMaybes
                 [("enabled" .=) <$> _cakEnabled,
                  ("name" .=) <$> _cakName,
                  ("stageKeys" .=) <$> _cakStageKeys,
                  ("description" .=) <$> _cakDescription])

instance ToPath CreateAPIKey where
        toPath = const "/apikeys"

instance ToQuery CreateAPIKey where
        toQuery = const mempty
