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
-- Module      : Network.AWS.SNS.GetSMSAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the settings for sending SMS messages from your account.
--
--
-- These settings are set with the @SetSMSAttributes@ action.
--
module Network.AWS.SNS.GetSMSAttributes
    (
    -- * Creating a Request
      getSMSAttributes
    , GetSMSAttributes
    -- * Request Lenses
    , gsmsaAttributes

    -- * Destructuring the Response
    , getSMSAttributesResponse
    , GetSMSAttributesResponse
    -- * Response Lenses
    , gsmsarsAttributes
    , gsmsarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | The input for the @GetSMSAttributes@ request.
--
--
--
-- /See:/ 'getSMSAttributes' smart constructor.
newtype GetSMSAttributes = GetSMSAttributes'
  { _gsmsaAttributes :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSMSAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsmsaAttributes' - A list of the individual attribute names, such as @MonthlySpendLimit@ , for which you want values. For all attribute names, see <http://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes> . If you don't use this parameter, Amazon SNS returns all SMS attributes.
getSMSAttributes
    :: GetSMSAttributes
getSMSAttributes = GetSMSAttributes' {_gsmsaAttributes = Nothing}


-- | A list of the individual attribute names, such as @MonthlySpendLimit@ , for which you want values. For all attribute names, see <http://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes> . If you don't use this parameter, Amazon SNS returns all SMS attributes.
gsmsaAttributes :: Lens' GetSMSAttributes [Text]
gsmsaAttributes = lens _gsmsaAttributes (\ s a -> s{_gsmsaAttributes = a}) . _Default . _Coerce

instance AWSRequest GetSMSAttributes where
        type Rs GetSMSAttributes = GetSMSAttributesResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "GetSMSAttributesResult"
              (\ s h x ->
                 GetSMSAttributesResponse' <$>
                   (x .@? "attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetSMSAttributes where

instance NFData GetSMSAttributes where

instance ToHeaders GetSMSAttributes where
        toHeaders = const mempty

instance ToPath GetSMSAttributes where
        toPath = const "/"

instance ToQuery GetSMSAttributes where
        toQuery GetSMSAttributes'{..}
          = mconcat
              ["Action" =: ("GetSMSAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "attributes" =:
                 toQuery (toQueryList "member" <$> _gsmsaAttributes)]

-- | The response from the @GetSMSAttributes@ request.
--
--
--
-- /See:/ 'getSMSAttributesResponse' smart constructor.
data GetSMSAttributesResponse = GetSMSAttributesResponse'
  { _gsmsarsAttributes     :: !(Maybe (Map Text Text))
  , _gsmsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSMSAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsmsarsAttributes' - The SMS attribute names and their values.
--
-- * 'gsmsarsResponseStatus' - -- | The response status code.
getSMSAttributesResponse
    :: Int -- ^ 'gsmsarsResponseStatus'
    -> GetSMSAttributesResponse
getSMSAttributesResponse pResponseStatus_ =
  GetSMSAttributesResponse'
    {_gsmsarsAttributes = Nothing, _gsmsarsResponseStatus = pResponseStatus_}


-- | The SMS attribute names and their values.
gsmsarsAttributes :: Lens' GetSMSAttributesResponse (HashMap Text Text)
gsmsarsAttributes = lens _gsmsarsAttributes (\ s a -> s{_gsmsarsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gsmsarsResponseStatus :: Lens' GetSMSAttributesResponse Int
gsmsarsResponseStatus = lens _gsmsarsResponseStatus (\ s a -> s{_gsmsarsResponseStatus = a})

instance NFData GetSMSAttributesResponse where
