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
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups the specified user belongs to.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html AWS API Reference> for ListGroupsForUser.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Creating a Request
      listGroupsForUser
    , ListGroupsForUser
    -- * Request Lenses
    , lgfuMarker
    , lgfuMaxItems
    , lgfuUserName

    -- * Destructuring the Response
    , listGroupsForUserResponse
    , ListGroupsForUserResponse
    -- * Response Lenses
    , lgfursMarker
    , lgfursIsTruncated
    , lgfursResponseStatus
    , lgfursGroups
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
    { _lgfuMarker   :: !(Maybe Text)
    , _lgfuMaxItems :: !(Maybe Nat)
    , _lgfuUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupsForUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgfuMarker'
--
-- * 'lgfuMaxItems'
--
-- * 'lgfuUserName'
listGroupsForUser
    :: Text -- ^ 'lgfuUserName'
    -> ListGroupsForUser
listGroupsForUser pUserName_ =
    ListGroupsForUser'
    { _lgfuMarker = Nothing
    , _lgfuMaxItems = Nothing
    , _lgfuUserName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the 'Marker' element in the response you received to inform
-- the next call about where to start.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\ s a -> s{_lgfuMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. If this is the case, the 'IsTruncated' response
-- element returns 'true' and 'Marker' contains a value to include in the
-- subsequent call that tells the service where to continue from.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Natural)
lgfuMaxItems = lens _lgfuMaxItems (\ s a -> s{_lgfuMaxItems = a}) . mapping _Nat;

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\ s a -> s{_lgfuUserName = a});

instance AWSPager ListGroupsForUser where
        page rq rs
          | stop (rs ^. lgfursIsTruncated) = Nothing
          | isNothing (rs ^. lgfursMarker) = Nothing
          | otherwise =
            Just $ rq & lgfuMarker .~ rs ^. lgfursMarker

instance AWSRequest ListGroupsForUser where
        type Rs ListGroupsForUser = ListGroupsForUserResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListGroupsForUserResult"
              (\ s h x ->
                 ListGroupsForUserResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Groups" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListGroupsForUser where
        toHeaders = const mempty

instance ToPath ListGroupsForUser where
        toPath = const "/"

instance ToQuery ListGroupsForUser where
        toQuery ListGroupsForUser'{..}
          = mconcat
              ["Action" =: ("ListGroupsForUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lgfuMarker, "MaxItems" =: _lgfuMaxItems,
               "UserName" =: _lgfuUserName]

-- | Contains the response to a successful ListGroupsForUser request.
--
-- /See:/ 'listGroupsForUserResponse' smart constructor.
data ListGroupsForUserResponse = ListGroupsForUserResponse'
    { _lgfursMarker         :: !(Maybe Text)
    , _lgfursIsTruncated    :: !(Maybe Bool)
    , _lgfursResponseStatus :: !Int
    , _lgfursGroups         :: ![Group]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupsForUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgfursMarker'
--
-- * 'lgfursIsTruncated'
--
-- * 'lgfursResponseStatus'
--
-- * 'lgfursGroups'
listGroupsForUserResponse
    :: Int -- ^ 'lgfursResponseStatus'
    -> ListGroupsForUserResponse
listGroupsForUserResponse pResponseStatus_ =
    ListGroupsForUserResponse'
    { _lgfursMarker = Nothing
    , _lgfursIsTruncated = Nothing
    , _lgfursResponseStatus = pResponseStatus_
    , _lgfursGroups = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lgfursMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfursMarker = lens _lgfursMarker (\ s a -> s{_lgfursMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items. Note that
-- IAM might return fewer than the 'MaxItems' number of results even when
-- there are more results available. We recommend that you check
-- 'IsTruncated' after every call to ensure that you receive all of your
-- results.
lgfursIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfursIsTruncated = lens _lgfursIsTruncated (\ s a -> s{_lgfursIsTruncated = a});

-- | The response status code.
lgfursResponseStatus :: Lens' ListGroupsForUserResponse Int
lgfursResponseStatus = lens _lgfursResponseStatus (\ s a -> s{_lgfursResponseStatus = a});

-- | A list of groups.
lgfursGroups :: Lens' ListGroupsForUserResponse [Group]
lgfursGroups = lens _lgfursGroups (\ s a -> s{_lgfursGroups = a}) . _Coerce;
