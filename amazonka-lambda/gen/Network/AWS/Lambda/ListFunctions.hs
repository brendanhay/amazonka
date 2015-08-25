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
-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your Lambda functions. For each function, the response
-- includes the function configuration information. You must use
-- GetFunction to retrieve the code for your function.
--
-- This operation requires permission for the 'lambda:ListFunctions'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html AWS API Reference> for ListFunctions.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctions
    (
    -- * Creating a Request
      listFunctions
    , ListFunctions
    -- * Request Lenses
    , lfMaxItems
    , lfMarker

    -- * Destructuring the Response
    , listFunctionsResponse
    , ListFunctionsResponse
    -- * Response Lenses
    , lfrsNextMarker
    , lfrsFunctions
    , lfrsStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listFunctions' smart constructor.
data ListFunctions = ListFunctions'
    { _lfMaxItems :: !(Maybe Nat)
    , _lfMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFunctions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfMaxItems'
--
-- * 'lfMarker'
listFunctions
    :: ListFunctions
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
-- 'ListFunctions' operation. If present, indicates where to continue the
-- listing.
lfMarker :: Lens' ListFunctions (Maybe Text)
lfMarker = lens _lfMarker (\ s a -> s{_lfMarker = a});

instance AWSPager ListFunctions where
        page rq rs
          | stop (rs ^. lfrsNextMarker) = Nothing
          | stop (rs ^. lfrsFunctions) = Nothing
          | otherwise =
            Just $ rq & lfMarker .~ rs ^. lfrsNextMarker

instance AWSRequest ListFunctions where
        type Rs ListFunctions = ListFunctionsResponse
        request = get lambda
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
data ListFunctionsResponse = ListFunctionsResponse'
    { _lfrsNextMarker :: !(Maybe Text)
    , _lfrsFunctions  :: !(Maybe [FunctionConfiguration])
    , _lfrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFunctionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextMarker'
--
-- * 'lfrsFunctions'
--
-- * 'lfrsStatus'
listFunctionsResponse
    :: Int -- ^ 'lfrsStatus'
    -> ListFunctionsResponse
listFunctionsResponse pStatus_ =
    ListFunctionsResponse'
    { _lfrsNextMarker = Nothing
    , _lfrsFunctions = Nothing
    , _lfrsStatus = pStatus_
    }

-- | A string, present if there are more functions.
lfrsNextMarker :: Lens' ListFunctionsResponse (Maybe Text)
lfrsNextMarker = lens _lfrsNextMarker (\ s a -> s{_lfrsNextMarker = a});

-- | A list of Lambda functions.
lfrsFunctions :: Lens' ListFunctionsResponse [FunctionConfiguration]
lfrsFunctions = lens _lfrsFunctions (\ s a -> s{_lfrsFunctions = a}) . _Default . _Coerce;

-- | The response status code.
lfrsStatus :: Lens' ListFunctionsResponse Int
lfrsStatus = lens _lfrsStatus (\ s a -> s{_lfrsStatus = a});
