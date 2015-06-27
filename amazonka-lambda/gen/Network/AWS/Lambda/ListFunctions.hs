{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of your Lambda functions. For each function, the response
-- includes the function configuration information. You must use
-- GetFunction to retrieve the code for your function.
--
-- This operation requires permission for the @lambda:ListFunctions@
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html>
module Network.AWS.Lambda.ListFunctions
    (
    -- * Request
      ListFunctions
    -- ** Request constructor
    , listFunctions
    -- ** Request lenses
    , lfMaxItems
    , lfMarker

    -- * Response
    , ListFunctionsResponse
    -- ** Response constructor
    , listFunctionsResponse
    -- ** Response lenses
    , lfrNextMarker
    , lfrFunctions
    , lfrStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listFunctions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfMaxItems'
--
-- * 'lfMarker'
data ListFunctions = ListFunctions'
    { _lfMaxItems :: !(Maybe Nat)
    , _lfMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListFunctions' smart constructor.
listFunctions :: ListFunctions
listFunctions =
    ListFunctions'
    { _lfMaxItems = Nothing
    , _lfMarker = Nothing
    }

-- | Optional integer. Specifies the maximum number of AWS Lambda functions
-- to return in response. This parameter value must be greater than 0.
lfMaxItems :: Lens' ListFunctions (Maybe Natural)
lfMaxItems = lens _lfMaxItems (\ s a -> s{_lfMaxItems = a}) . mapping _Nat;

-- | Optional string. An opaque pagination token returned from a previous
-- @ListFunctions@ operation. If present, indicates where to continue the
-- listing.
lfMarker :: Lens' ListFunctions (Maybe Text)
lfMarker = lens _lfMarker (\ s a -> s{_lfMarker = a});

instance AWSPager ListFunctions where
        page rq rs
          | stop (rs ^. lfrNextMarker) = Nothing
          | stop (rs ^. lfrFunctions) = Nothing
          | otherwise =
            Just $ rq & lfMarker .~ rs ^. lfrNextMarker

instance AWSRequest ListFunctions where
        type Sv ListFunctions = Lambda
        type Rs ListFunctions = ListFunctionsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListFunctionsResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "Functions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListFunctions where
        toHeaders = const mempty

instance ToPath ListFunctions where
        toPath = const "/2015-03-31/functions/"

instance ToQuery ListFunctions where
        toQuery ListFunctions'{..}
          = mconcat
              ["MaxItems" =: _lfMaxItems, "Marker" =: _lfMarker]

-- | Contains a list of AWS Lambda function configurations (see
-- FunctionConfiguration.
--
-- /See:/ 'listFunctionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfrNextMarker'
--
-- * 'lfrFunctions'
--
-- * 'lfrStatus'
data ListFunctionsResponse = ListFunctionsResponse'
    { _lfrNextMarker :: !(Maybe Text)
    , _lfrFunctions  :: !(Maybe [FunctionConfiguration])
    , _lfrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListFunctionsResponse' smart constructor.
listFunctionsResponse :: Int -> ListFunctionsResponse
listFunctionsResponse pStatus =
    ListFunctionsResponse'
    { _lfrNextMarker = Nothing
    , _lfrFunctions = Nothing
    , _lfrStatus = pStatus
    }

-- | A string, present if there are more functions.
lfrNextMarker :: Lens' ListFunctionsResponse (Maybe Text)
lfrNextMarker = lens _lfrNextMarker (\ s a -> s{_lfrNextMarker = a});

-- | A list of Lambda functions.
lfrFunctions :: Lens' ListFunctionsResponse [FunctionConfiguration]
lfrFunctions = lens _lfrFunctions (\ s a -> s{_lfrFunctions = a}) . _Default;

-- | FIXME: Undocumented member.
lfrStatus :: Lens' ListFunctionsResponse Int
lfrStatus = lens _lfrStatus (\ s a -> s{_lfrStatus = a});
