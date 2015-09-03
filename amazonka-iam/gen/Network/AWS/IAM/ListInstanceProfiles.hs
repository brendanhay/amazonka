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
-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified path prefix. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfiles.html AWS API Reference> for ListInstanceProfiles.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfiles
    (
    -- * Creating a Request
      listInstanceProfiles
    , ListInstanceProfiles
    -- * Request Lenses
    , lipPathPrefix
    , lipMarker
    , lipMaxItems

    -- * Destructuring the Response
    , listInstanceProfilesResponse
    , ListInstanceProfilesResponse
    -- * Response Lenses
    , liprsMarker
    , liprsIsTruncated
    , liprsResponseStatus
    , liprsInstanceProfiles
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
    { _lipPathPrefix :: !(Maybe Text)
    , _lipMarker     :: !(Maybe Text)
    , _lipMaxItems   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstanceProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipPathPrefix'
--
-- * 'lipMarker'
--
-- * 'lipMaxItems'
listInstanceProfiles
    :: ListInstanceProfiles
listInstanceProfiles =
    ListInstanceProfiles'
    { _lipPathPrefix = Nothing
    , _lipMarker = Nothing
    , _lipMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix
-- '\/application_abc\/component_xyz\/' gets all instance profiles whose
-- path starts with '\/application_abc\/component_xyz\/'.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all instance profiles.
lipPathPrefix :: Lens' ListInstanceProfiles (Maybe Text)
lipPathPrefix = lens _lipPathPrefix (\ s a -> s{_lipPathPrefix = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lipMarker :: Lens' ListInstanceProfiles (Maybe Text)
lipMarker = lens _lipMarker (\ s a -> s{_lipMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lipMaxItems :: Lens' ListInstanceProfiles (Maybe Natural)
lipMaxItems = lens _lipMaxItems (\ s a -> s{_lipMaxItems = a}) . mapping _Nat;

instance AWSPager ListInstanceProfiles where
        page rq rs
          | stop (rs ^. liprsMarker) = Nothing
          | stop (rs ^. liprsInstanceProfiles) = Nothing
          | otherwise =
            Just $ rq & lipMarker .~ rs ^. liprsMarker

instance AWSRequest ListInstanceProfiles where
        type Rs ListInstanceProfiles =
             ListInstanceProfilesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListInstanceProfilesResult"
              (\ s h x ->
                 ListInstanceProfilesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "InstanceProfiles" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListInstanceProfiles where
        toHeaders = const mempty

instance ToPath ListInstanceProfiles where
        toPath = const "/"

instance ToQuery ListInstanceProfiles where
        toQuery ListInstanceProfiles'{..}
          = mconcat
              ["Action" =: ("ListInstanceProfiles" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lipPathPrefix,
               "Marker" =: _lipMarker, "MaxItems" =: _lipMaxItems]

-- | Contains the response to a successful ListInstanceProfiles request.
--
-- /See:/ 'listInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
    { _liprsMarker           :: !(Maybe Text)
    , _liprsIsTruncated      :: !(Maybe Bool)
    , _liprsResponseStatus   :: !Int
    , _liprsInstanceProfiles :: ![InstanceProfile]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstanceProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liprsMarker'
--
-- * 'liprsIsTruncated'
--
-- * 'liprsResponseStatus'
--
-- * 'liprsInstanceProfiles'
listInstanceProfilesResponse
    :: Int -- ^ 'liprsResponseStatus'
    -> ListInstanceProfilesResponse
listInstanceProfilesResponse pResponseStatus_ =
    ListInstanceProfilesResponse'
    { _liprsMarker = Nothing
    , _liprsIsTruncated = Nothing
    , _liprsResponseStatus = pResponseStatus_
    , _liprsInstanceProfiles = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
liprsMarker :: Lens' ListInstanceProfilesResponse (Maybe Text)
liprsMarker = lens _liprsMarker (\ s a -> s{_liprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
liprsIsTruncated :: Lens' ListInstanceProfilesResponse (Maybe Bool)
liprsIsTruncated = lens _liprsIsTruncated (\ s a -> s{_liprsIsTruncated = a});

-- | The response status code.
liprsResponseStatus :: Lens' ListInstanceProfilesResponse Int
liprsResponseStatus = lens _liprsResponseStatus (\ s a -> s{_liprsResponseStatus = a});

-- | A list of instance profiles.
liprsInstanceProfiles :: Lens' ListInstanceProfilesResponse [InstanceProfile]
liprsInstanceProfiles = lens _liprsInstanceProfiles (\ s a -> s{_liprsInstanceProfiles = a}) . _Coerce;
