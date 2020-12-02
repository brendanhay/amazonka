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
-- Module      : Network.AWS.Shield.ListProtections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all 'Protection' objects for the account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListProtections
    (
    -- * Creating a Request
      listProtections
    , ListProtections
    -- * Request Lenses
    , lpNextToken
    , lpMaxResults

    -- * Destructuring the Response
    , listProtectionsResponse
    , ListProtectionsResponse
    -- * Response Lenses
    , lprsProtections
    , lprsNextToken
    , lprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'listProtections' smart constructor.
data ListProtections = ListProtections'
  { _lpNextToken  :: !(Maybe Text)
  , _lpMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProtections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
--
-- * 'lpMaxResults' - The maximum number of 'Protection' objects to be returned. If this is left blank the first 20 results will be returned.
listProtections
    :: ListProtections
listProtections =
  ListProtections' {_lpNextToken = Nothing, _lpMaxResults = Nothing}


-- | The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
lpNextToken :: Lens' ListProtections (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | The maximum number of 'Protection' objects to be returned. If this is left blank the first 20 results will be returned.
lpMaxResults :: Lens' ListProtections (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a}) . mapping _Nat

instance AWSPager ListProtections where
        page rq rs
          | stop (rs ^. lprsNextToken) = Nothing
          | stop (rs ^. lprsProtections) = Nothing
          | otherwise =
            Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest ListProtections where
        type Rs ListProtections = ListProtectionsResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 ListProtectionsResponse' <$>
                   (x .?> "Protections" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListProtections where

instance NFData ListProtections where

instance ToHeaders ListProtections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.ListProtections" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProtections where
        toJSON ListProtections'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpNextToken,
                  ("MaxResults" .=) <$> _lpMaxResults])

instance ToPath ListProtections where
        toPath = const "/"

instance ToQuery ListProtections where
        toQuery = const mempty

-- | /See:/ 'listProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { _lprsProtections    :: !(Maybe [Protection])
  , _lprsNextToken      :: !(Maybe Text)
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProtectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsProtections' - The array of enabled 'Protection' objects.
--
-- * 'lprsNextToken' - If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listProtectionsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListProtectionsResponse
listProtectionsResponse pResponseStatus_ =
  ListProtectionsResponse'
    { _lprsProtections = Nothing
    , _lprsNextToken = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | The array of enabled 'Protection' objects.
lprsProtections :: Lens' ListProtectionsResponse [Protection]
lprsProtections = lens _lprsProtections (\ s a -> s{_lprsProtections = a}) . _Default . _Coerce

-- | If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
lprsNextToken :: Lens' ListProtectionsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListProtectionsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListProtectionsResponse where
