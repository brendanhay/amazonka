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
-- < GetFunction> to retrieve the code for your function.
--
-- This operation requires permission for the 'lambda:ListFunctions'
-- action.
--
-- If you are using versioning feature, the response returns list of
-- $LATEST versions of your functions. For information about the versioning
-- feature, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases>.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctions
    (
    -- * Creating a Request
      listFunctions
    , ListFunctions
    -- * Request Lenses
    , lfMarker
    , lfMaxItems

    -- * Destructuring the Response
    , listFunctionsResponse
    , ListFunctionsResponse
    -- * Response Lenses
    , lfrsNextMarker
    , lfrsFunctions
    , lfrsResponseStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listFunctions' smart constructor.
data ListFunctions = ListFunctions'
    { _lfMarker   :: !(Maybe Text)
    , _lfMaxItems :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFunctions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfMarker'
--
-- * 'lfMaxItems'
listFunctions
    :: ListFunctions
listFunctions =
    ListFunctions'
    { _lfMarker = Nothing
    , _lfMaxItems = Nothing
    }

-- | Optional string. An opaque pagination token returned from a previous
-- 'ListFunctions' operation. If present, indicates where to continue the
-- listing.
lfMarker :: Lens' ListFunctions (Maybe Text)
lfMarker = lens _lfMarker (\ s a -> s{_lfMarker = a});

-- | Optional integer. Specifies the maximum number of AWS Lambda functions
-- to return in response. This parameter value must be greater than 0.
lfMaxItems :: Lens' ListFunctions (Maybe Natural)
lfMaxItems = lens _lfMaxItems (\ s a -> s{_lfMaxItems = a}) . mapping _Nat;

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
              ["Marker" =: _lfMarker, "MaxItems" =: _lfMaxItems]

-- | Contains a list of AWS Lambda function configurations (see
-- < FunctionConfiguration>.
--
-- /See:/ 'listFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
    { _lfrsNextMarker     :: !(Maybe Text)
    , _lfrsFunctions      :: !(Maybe [FunctionConfiguration])
    , _lfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListFunctionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextMarker'
--
-- * 'lfrsFunctions'
--
-- * 'lfrsResponseStatus'
listFunctionsResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFunctionsResponse
listFunctionsResponse pResponseStatus_ =
    ListFunctionsResponse'
    { _lfrsNextMarker = Nothing
    , _lfrsFunctions = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }

-- | A string, present if there are more functions.
lfrsNextMarker :: Lens' ListFunctionsResponse (Maybe Text)
lfrsNextMarker = lens _lfrsNextMarker (\ s a -> s{_lfrsNextMarker = a});

-- | A list of Lambda functions.
lfrsFunctions :: Lens' ListFunctionsResponse [FunctionConfiguration]
lfrsFunctions = lens _lfrsFunctions (\ s a -> s{_lfrsFunctions = a}) . _Default . _Coerce;

-- | The response status code.
lfrsResponseStatus :: Lens' ListFunctionsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a});
