{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups the specified user belongs to.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html>
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Request
      ListGroupsForUser
    -- ** Request constructor
    , listGroupsForUser
    -- ** Request lenses
    , lgfuMaxItems
    , lgfuMarker
    , lgfuUserName

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response constructor
    , listGroupsForUserResponse
    -- ** Response lenses
    , lgfurMarker
    , lgfurIsTruncated
    , lgfurStatus
    , lgfurGroups
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroupsForUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfuMaxItems'
--
-- * 'lgfuMarker'
--
-- * 'lgfuUserName'
data ListGroupsForUser = ListGroupsForUser'
    { _lgfuMaxItems :: !(Maybe Nat)
    , _lgfuMarker   :: !(Maybe Text)
    , _lgfuUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupsForUser' smart constructor.
listGroupsForUser :: Text -> ListGroupsForUser
listGroupsForUser pUserName =
    ListGroupsForUser'
    { _lgfuMaxItems = Nothing
    , _lgfuMarker = Nothing
    , _lgfuUserName = pUserName
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Natural)
lgfuMaxItems = lens _lgfuMaxItems (\ s a -> s{_lgfuMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\ s a -> s{_lgfuMarker = a});

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\ s a -> s{_lgfuUserName = a});

instance AWSPager ListGroupsForUser where
        page rq rs
          | stop (rs ^. lgfurIsTruncated) = Nothing
          | isNothing (rs ^. lgfurMarker) = Nothing
          | otherwise =
            Just $ rq & lgfuMarker .~ rs ^. lgfurMarker

instance AWSRequest ListGroupsForUser where
        type Sv ListGroupsForUser = IAM
        type Rs ListGroupsForUser = ListGroupsForUserResponse
        request = post
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
               "MaxItems" =: _lgfuMaxItems, "Marker" =: _lgfuMarker,
               "UserName" =: _lgfuUserName]

-- | Contains the response to a successful ListGroupsForUser request.
--
-- /See:/ 'listGroupsForUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfurMarker'
--
-- * 'lgfurIsTruncated'
--
-- * 'lgfurStatus'
--
-- * 'lgfurGroups'
data ListGroupsForUserResponse = ListGroupsForUserResponse'
    { _lgfurMarker      :: !(Maybe Text)
    , _lgfurIsTruncated :: !(Maybe Bool)
    , _lgfurStatus      :: !Int
    , _lgfurGroups      :: ![Group]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupsForUserResponse' smart constructor.
listGroupsForUserResponse :: Int -> ListGroupsForUserResponse
listGroupsForUserResponse pStatus =
    ListGroupsForUserResponse'
    { _lgfurMarker = Nothing
    , _lgfurIsTruncated = Nothing
    , _lgfurStatus = pStatus
    , _lgfurGroups = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgfurMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfurMarker = lens _lgfurMarker (\ s a -> s{_lgfurMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lgfurIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfurIsTruncated = lens _lgfurIsTruncated (\ s a -> s{_lgfurIsTruncated = a});

-- | FIXME: Undocumented member.
lgfurStatus :: Lens' ListGroupsForUserResponse Int
lgfurStatus = lens _lgfurStatus (\ s a -> s{_lgfurStatus = a});

-- | A list of groups.
lgfurGroups :: Lens' ListGroupsForUserResponse [Group]
lgfurGroups = lens _lgfurGroups (\ s a -> s{_lgfurGroups = a});
