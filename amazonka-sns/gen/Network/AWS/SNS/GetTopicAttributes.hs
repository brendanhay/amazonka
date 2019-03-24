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
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned might differ based on the authorization of the user.
--
--
module Network.AWS.SNS.GetTopicAttributes
    (
    -- * Creating a Request
      getTopicAttributes
    , GetTopicAttributes
    -- * Request Lenses
    , gtaTopicARN

    -- * Destructuring the Response
    , getTopicAttributesResponse
    , GetTopicAttributesResponse
    -- * Response Lenses
    , gtarsAttributes
    , gtarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for GetTopicAttributes action.
--
--
--
-- /See:/ 'getTopicAttributes' smart constructor.
newtype GetTopicAttributes = GetTopicAttributes'
  { _gtaTopicARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTopicAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtaTopicARN' - The ARN of the topic whose properties you want to get.
getTopicAttributes
    :: Text -- ^ 'gtaTopicARN'
    -> GetTopicAttributes
getTopicAttributes pTopicARN_ = GetTopicAttributes' {_gtaTopicARN = pTopicARN_}


-- | The ARN of the topic whose properties you want to get.
gtaTopicARN :: Lens' GetTopicAttributes Text
gtaTopicARN = lens _gtaTopicARN (\ s a -> s{_gtaTopicARN = a})

instance AWSRequest GetTopicAttributes where
        type Rs GetTopicAttributes =
             GetTopicAttributesResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "GetTopicAttributesResult"
              (\ s h x ->
                 GetTopicAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetTopicAttributes where

instance NFData GetTopicAttributes where

instance ToHeaders GetTopicAttributes where
        toHeaders = const mempty

instance ToPath GetTopicAttributes where
        toPath = const "/"

instance ToQuery GetTopicAttributes where
        toQuery GetTopicAttributes'{..}
          = mconcat
              ["Action" =: ("GetTopicAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _gtaTopicARN]

-- | Response for GetTopicAttributes action.
--
--
--
-- /See:/ 'getTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
  { _gtarsAttributes     :: !(Maybe (Map Text Text))
  , _gtarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTopicAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtarsAttributes' - A map of the topic's attributes. Attributes in this map include the following:     * @TopicArn@
