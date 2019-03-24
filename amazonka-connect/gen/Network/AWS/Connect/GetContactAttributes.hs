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
-- Module      : Network.AWS.Connect.GetContactAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contact attributes associated with a contact.
--
--
module Network.AWS.Connect.GetContactAttributes
    (
    -- * Creating a Request
      getContactAttributes
    , GetContactAttributes
    -- * Request Lenses
    , gcaInstanceId
    , gcaInitialContactId

    -- * Destructuring the Response
    , getContactAttributesResponse
    , GetContactAttributesResponse
    -- * Response Lenses
    , gcarsAttributes
    , gcarsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContactAttributes' smart constructor.
data GetContactAttributes = GetContactAttributes'
  { _gcaInstanceId       :: !Text
  , _gcaInitialContactId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContactAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcaInstanceId' - The instance ID for the instance from which to retrieve contact attributes.
--
-- * 'gcaInitialContactId' - The ID for the initial contact in Amazon Connect associated with the attributes to update.
getContactAttributes
    :: Text -- ^ 'gcaInstanceId'
    -> Text -- ^ 'gcaInitialContactId'
    -> GetContactAttributes
getContactAttributes pInstanceId_ pInitialContactId_ =
  GetContactAttributes'
    {_gcaInstanceId = pInstanceId_, _gcaInitialContactId = pInitialContactId_}


-- | The instance ID for the instance from which to retrieve contact attributes.
gcaInstanceId :: Lens' GetContactAttributes Text
gcaInstanceId = lens _gcaInstanceId (\ s a -> s{_gcaInstanceId = a})

-- | The ID for the initial contact in Amazon Connect associated with the attributes to update.
gcaInitialContactId :: Lens' GetContactAttributes Text
gcaInitialContactId = lens _gcaInitialContactId (\ s a -> s{_gcaInitialContactId = a})

instance AWSRequest GetContactAttributes where
        type Rs GetContactAttributes =
             GetContactAttributesResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 GetContactAttributesResponse' <$>
                   (x .?> "Attributes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetContactAttributes where

instance NFData GetContactAttributes where

instance ToHeaders GetContactAttributes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetContactAttributes where
        toPath GetContactAttributes'{..}
          = mconcat
              ["/contact/attributes/", toBS _gcaInstanceId, "/",
               toBS _gcaInitialContactId]

instance ToQuery GetContactAttributes where
        toQuery = const mempty

-- | /See:/ 'getContactAttributesResponse' smart constructor.
data GetContactAttributesResponse = GetContactAttributesResponse'
  { _gcarsAttributes     :: !(Maybe (Map Text Text))
  , _gcarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContactAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcarsAttributes' - The attributes to update.
--
-- * 'gcarsResponseStatus' - -- | The response status code.
getContactAttributesResponse
    :: Int -- ^ 'gcarsResponseStatus'
    -> GetContactAttributesResponse
getContactAttributesResponse pResponseStatus_ =
  GetContactAttributesResponse'
    {_gcarsAttributes = Nothing, _gcarsResponseStatus = pResponseStatus_}


-- | The attributes to update.
gcarsAttributes :: Lens' GetContactAttributesResponse (HashMap Text Text)
gcarsAttributes = lens _gcarsAttributes (\ s a -> s{_gcarsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gcarsResponseStatus :: Lens' GetContactAttributesResponse Int
gcarsResponseStatus = lens _gcarsResponseStatus (\ s a -> s{_gcarsResponseStatus = a})

instance NFData GetContactAttributesResponse where
