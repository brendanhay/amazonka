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
-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfilesForRole.html AWS API Reference> for ListInstanceProfilesForRole.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfilesForRole
    (
    -- * Creating a Request
      listInstanceProfilesForRole
    , ListInstanceProfilesForRole
    -- * Request Lenses
    , lipfrMarker
    , lipfrMaxItems
    , lipfrRoleName

    -- * Destructuring the Response
    , listInstanceProfilesForRoleResponse
    , ListInstanceProfilesForRoleResponse
    -- * Response Lenses
    , lipfrrsMarker
    , lipfrrsIsTruncated
    , lipfrrsResponseStatus
    , lipfrrsInstanceProfiles
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listInstanceProfilesForRole' smart constructor.
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'
    { _lipfrMarker   :: !(Maybe Text)
    , _lipfrMaxItems :: !(Maybe Nat)
    , _lipfrRoleName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstanceProfilesForRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipfrMarker'
--
-- * 'lipfrMaxItems'
--
-- * 'lipfrRoleName'
listInstanceProfilesForRole
    :: Text -- ^ 'lipfrRoleName'
    -> ListInstanceProfilesForRole
listInstanceProfilesForRole pRoleName_ =
    ListInstanceProfilesForRole'
    { _lipfrMarker = Nothing
    , _lipfrMaxItems = Nothing
    , _lipfrRoleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lipfrMarker :: Lens' ListInstanceProfilesForRole (Maybe Text)
lipfrMarker = lens _lipfrMarker (\ s a -> s{_lipfrMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lipfrMaxItems :: Lens' ListInstanceProfilesForRole (Maybe Natural)
lipfrMaxItems = lens _lipfrMaxItems (\ s a -> s{_lipfrMaxItems = a}) . mapping _Nat;

-- | The name of the role to list instance profiles for.
lipfrRoleName :: Lens' ListInstanceProfilesForRole Text
lipfrRoleName = lens _lipfrRoleName (\ s a -> s{_lipfrRoleName = a});

instance AWSPager ListInstanceProfilesForRole where
        page rq rs
          | stop (rs ^. lipfrrsMarker) = Nothing
          | stop (rs ^. lipfrrsInstanceProfiles) = Nothing
          | otherwise =
            Just $ rq & lipfrMarker .~ rs ^. lipfrrsMarker

instance AWSRequest ListInstanceProfilesForRole where
        type Rs ListInstanceProfilesForRole =
             ListInstanceProfilesForRoleResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper
              "ListInstanceProfilesForRoleResult"
              (\ s h x ->
                 ListInstanceProfilesForRoleResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "InstanceProfiles" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListInstanceProfilesForRole where
        toHeaders = const mempty

instance ToPath ListInstanceProfilesForRole where
        toPath = const "/"

instance ToQuery ListInstanceProfilesForRole where
        toQuery ListInstanceProfilesForRole'{..}
          = mconcat
              ["Action" =:
                 ("ListInstanceProfilesForRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lipfrMarker,
               "MaxItems" =: _lipfrMaxItems,
               "RoleName" =: _lipfrRoleName]

-- | Contains the response to a successful ListInstanceProfilesForRole
-- request.
--
-- /See:/ 'listInstanceProfilesForRoleResponse' smart constructor.
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'
    { _lipfrrsMarker           :: !(Maybe Text)
    , _lipfrrsIsTruncated      :: !(Maybe Bool)
    , _lipfrrsResponseStatus   :: !Int
    , _lipfrrsInstanceProfiles :: ![InstanceProfile]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstanceProfilesForRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipfrrsMarker'
--
-- * 'lipfrrsIsTruncated'
--
-- * 'lipfrrsResponseStatus'
--
-- * 'lipfrrsInstanceProfiles'
listInstanceProfilesForRoleResponse
    :: Int -- ^ 'lipfrrsResponseStatus'
    -> ListInstanceProfilesForRoleResponse
listInstanceProfilesForRoleResponse pResponseStatus_ =
    ListInstanceProfilesForRoleResponse'
    { _lipfrrsMarker = Nothing
    , _lipfrrsIsTruncated = Nothing
    , _lipfrrsResponseStatus = pResponseStatus_
    , _lipfrrsInstanceProfiles = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lipfrrsMarker :: Lens' ListInstanceProfilesForRoleResponse (Maybe Text)
lipfrrsMarker = lens _lipfrrsMarker (\ s a -> s{_lipfrrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lipfrrsIsTruncated :: Lens' ListInstanceProfilesForRoleResponse (Maybe Bool)
lipfrrsIsTruncated = lens _lipfrrsIsTruncated (\ s a -> s{_lipfrrsIsTruncated = a});

-- | The response status code.
lipfrrsResponseStatus :: Lens' ListInstanceProfilesForRoleResponse Int
lipfrrsResponseStatus = lens _lipfrrsResponseStatus (\ s a -> s{_lipfrrsResponseStatus = a});

-- | A list of instance profiles.
lipfrrsInstanceProfiles :: Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrsInstanceProfiles = lens _lipfrrsInstanceProfiles (\ s a -> s{_lipfrrsInstanceProfiles = a}) . _Coerce;
