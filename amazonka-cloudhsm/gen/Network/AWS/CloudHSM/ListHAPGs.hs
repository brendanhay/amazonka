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
-- Module      : Network.AWS.CloudHSM.ListHAPGs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the high-availability partition groups for the account.
--
-- This operation supports pagination with the use of the /NextToken/
-- member. If more results are available, the /NextToken/ member of the
-- response contains a token that you pass in the next call to ListHapgs to
-- retrieve the next set of items.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListHAPGs.html AWS API Reference> for ListHAPGs.
module Network.AWS.CloudHSM.ListHAPGs
    (
    -- * Creating a Request
      listHAPGs
    , ListHAPGs
    -- * Request Lenses
    , lhNextToken

    -- * Destructuring the Response
    , listHAPGsResponse
    , ListHAPGsResponse
    -- * Response Lenses
    , lhrsNextToken
    , lhrsStatus
    , lhrsHAPGList
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listHAPGs' smart constructor.
newtype ListHAPGs = ListHAPGs'
    { _lhNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHAPGs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhNextToken'
listHAPGs
    :: ListHAPGs
listHAPGs =
    ListHAPGs'
    { _lhNextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to ListHapgs. Pass null if
-- this is the first call.
lhNextToken :: Lens' ListHAPGs (Maybe Text)
lhNextToken = lens _lhNextToken (\ s a -> s{_lhNextToken = a});

instance AWSRequest ListHAPGs where
        type Sv ListHAPGs = CloudHSM
        type Rs ListHAPGs = ListHAPGsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListHAPGsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "HapgList" .!@ mempty))

instance ToHeaders ListHAPGs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListHapgs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHAPGs where
        toJSON ListHAPGs'{..}
          = object
              (catMaybes [("NextToken" .=) <$> _lhNextToken])

instance ToPath ListHAPGs where
        toPath = const "/"

instance ToQuery ListHAPGs where
        toQuery = const mempty

-- | /See:/ 'listHAPGsResponse' smart constructor.
data ListHAPGsResponse = ListHAPGsResponse'
    { _lhrsNextToken :: !(Maybe Text)
    , _lhrsStatus    :: !Int
    , _lhrsHAPGList  :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHAPGsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhrsNextToken'
--
-- * 'lhrsStatus'
--
-- * 'lhrsHAPGList'
listHAPGsResponse
    :: Int -- ^ 'lhrsStatus'
    -> ListHAPGsResponse
listHAPGsResponse pStatus_ =
    ListHAPGsResponse'
    { _lhrsNextToken = Nothing
    , _lhrsStatus = pStatus_
    , _lhrsHAPGList = mempty
    }

-- | If not null, more results are available. Pass this value to ListHapgs to
-- retrieve the next set of items.
lhrsNextToken :: Lens' ListHAPGsResponse (Maybe Text)
lhrsNextToken = lens _lhrsNextToken (\ s a -> s{_lhrsNextToken = a});

-- | The response status code.
lhrsStatus :: Lens' ListHAPGsResponse Int
lhrsStatus = lens _lhrsStatus (\ s a -> s{_lhrsStatus = a});

-- | The list of high-availability partition groups.
lhrsHAPGList :: Lens' ListHAPGsResponse [Text]
lhrsHAPGList = lens _lhrsHAPGList (\ s a -> s{_lhrsHAPGList = a}) . _Coerce;
