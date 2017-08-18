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
-- Module      : Network.AWS.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the versions of a group.
module Network.AWS.Greengrass.ListGroupVersions
    (
    -- * Creating a Request
      listGroupVersions
    , ListGroupVersions
    -- * Request Lenses
    , lgvNextToken
    , lgvMaxResults
    , lgvGroupId

    -- * Destructuring the Response
    , listGroupVersionsResponse
    , ListGroupVersionsResponse
    -- * Response Lenses
    , lgvrsVersions
    , lgvrsNextToken
    , lgvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
    { _lgvNextToken  :: !(Maybe Text)
    , _lgvMaxResults :: !(Maybe Text)
    , _lgvGroupId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgvNextToken' - Specifies the pagination token used when iterating through a paginated request
--
-- * 'lgvMaxResults' - Specifies the maximum number of list results to be returned in this page
--
-- * 'lgvGroupId' - The unique Id of the AWS Greengrass Group
listGroupVersions
    :: Text -- ^ 'lgvGroupId'
    -> ListGroupVersions
listGroupVersions pGroupId_ =
    ListGroupVersions'
    { _lgvNextToken = Nothing
    , _lgvMaxResults = Nothing
    , _lgvGroupId = pGroupId_
    }

-- | Specifies the pagination token used when iterating through a paginated request
lgvNextToken :: Lens' ListGroupVersions (Maybe Text)
lgvNextToken = lens _lgvNextToken (\ s a -> s{_lgvNextToken = a});

-- | Specifies the maximum number of list results to be returned in this page
lgvMaxResults :: Lens' ListGroupVersions (Maybe Text)
lgvMaxResults = lens _lgvMaxResults (\ s a -> s{_lgvMaxResults = a});

-- | The unique Id of the AWS Greengrass Group
lgvGroupId :: Lens' ListGroupVersions Text
lgvGroupId = lens _lgvGroupId (\ s a -> s{_lgvGroupId = a});

instance AWSRequest ListGroupVersions where
        type Rs ListGroupVersions = ListGroupVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroupVersions

instance NFData ListGroupVersions

instance ToHeaders ListGroupVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListGroupVersions where
        toPath ListGroupVersions'{..}
          = mconcat
              ["/greengrass/groups/", toBS _lgvGroupId,
               "/versions"]

instance ToQuery ListGroupVersions where
        toQuery ListGroupVersions'{..}
          = mconcat
              ["NextToken" =: _lgvNextToken,
               "MaxResults" =: _lgvMaxResults]

-- | /See:/ 'listGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
    { _lgvrsVersions       :: !(Maybe [VersionInformation])
    , _lgvrsNextToken      :: !(Maybe Text)
    , _lgvrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgvrsVersions' - Versions
--
-- * 'lgvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lgvrsResponseStatus' - -- | The response status code.
listGroupVersionsResponse
    :: Int -- ^ 'lgvrsResponseStatus'
    -> ListGroupVersionsResponse
listGroupVersionsResponse pResponseStatus_ =
    ListGroupVersionsResponse'
    { _lgvrsVersions = Nothing
    , _lgvrsNextToken = Nothing
    , _lgvrsResponseStatus = pResponseStatus_
    }

-- | Versions
lgvrsVersions :: Lens' ListGroupVersionsResponse [VersionInformation]
lgvrsVersions = lens _lgvrsVersions (\ s a -> s{_lgvrsVersions = a}) . _Default . _Coerce;

-- | The token for the next set of results, or ''null'' if there are no additional results.
lgvrsNextToken :: Lens' ListGroupVersionsResponse (Maybe Text)
lgvrsNextToken = lens _lgvrsNextToken (\ s a -> s{_lgvrsNextToken = a});

-- | -- | The response status code.
lgvrsResponseStatus :: Lens' ListGroupVersionsResponse Int
lgvrsResponseStatus = lens _lgvrsResponseStatus (\ s a -> s{_lgvrsResponseStatus = a});

instance NFData ListGroupVersionsResponse
