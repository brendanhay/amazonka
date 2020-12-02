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
-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the identities (email addresses and domains) for your AWS account, regardless of verification status.
--
--
-- You can execute this operation no more than once per second.
--
--
-- This operation returns paginated results.
module Network.AWS.SES.ListIdentities
    (
    -- * Creating a Request
      listIdentities
    , ListIdentities
    -- * Request Lenses
    , liIdentityType
    , liNextToken
    , liMaxItems

    -- * Destructuring the Response
    , listIdentitiesResponse
    , ListIdentitiesResponse
    -- * Response Lenses
    , lirsNextToken
    , lirsResponseStatus
    , lirsIdentities
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.
--
--
--
-- /See:/ 'listIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { _liIdentityType :: !(Maybe IdentityType)
  , _liNextToken    :: !(Maybe Text)
  , _liMaxItems     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liIdentityType' - The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
--
-- * 'liNextToken' - The token to use for pagination.
--
-- * 'liMaxItems' - The maximum number of identities per page. Possible values are 1-1000 inclusive.
listIdentities
    :: ListIdentities
listIdentities =
  ListIdentities'
    {_liIdentityType = Nothing, _liNextToken = Nothing, _liMaxItems = Nothing}


-- | The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
liIdentityType :: Lens' ListIdentities (Maybe IdentityType)
liIdentityType = lens _liIdentityType (\ s a -> s{_liIdentityType = a})

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The maximum number of identities per page. Possible values are 1-1000 inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Int)
liMaxItems = lens _liMaxItems (\ s a -> s{_liMaxItems = a})

instance AWSPager ListIdentities where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsIdentities) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListIdentities where
        type Rs ListIdentities = ListIdentitiesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "ListIdentitiesResult"
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "Identities" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListIdentities where

instance NFData ListIdentities where

instance ToHeaders ListIdentities where
        toHeaders = const mempty

instance ToPath ListIdentities where
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery ListIdentities'{..}
          = mconcat
              ["Action" =: ("ListIdentities" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "IdentityType" =: _liIdentityType,
               "NextToken" =: _liNextToken,
               "MaxItems" =: _liMaxItems]

-- | A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.
--
--
--
-- /See:/ 'listIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { _lirsNextToken      :: !(Maybe Text)
  , _lirsResponseStatus :: !Int
  , _lirsIdentities     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsNextToken' - The token used for pagination.
--
-- * 'lirsResponseStatus' - -- | The response status code.
--
-- * 'lirsIdentities' - A list of identities.
listIdentitiesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListIdentitiesResponse
listIdentitiesResponse pResponseStatus_ =
  ListIdentitiesResponse'
    { _lirsNextToken = Nothing
    , _lirsResponseStatus = pResponseStatus_
    , _lirsIdentities = mempty
    }


-- | The token used for pagination.
lirsNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListIdentitiesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

-- | A list of identities.
lirsIdentities :: Lens' ListIdentitiesResponse [Text]
lirsIdentities = lens _lirsIdentities (\ s a -> s{_lirsIdentities = a}) . _Coerce

instance NFData ListIdentitiesResponse where
