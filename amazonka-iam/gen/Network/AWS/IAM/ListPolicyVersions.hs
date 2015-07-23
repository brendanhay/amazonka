{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the versions of the specified managed policy,
-- including the version that is set as the policy\'s default version.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicyVersions.html>
module Network.AWS.IAM.ListPolicyVersions
    (
    -- * Request
      ListPolicyVersions
    -- ** Request constructor
    , listPolicyVersions
    -- ** Request lenses
    , lpvrqMaxItems
    , lpvrqMarker
    , lpvrqPolicyARN

    -- * Response
    , ListPolicyVersionsResponse
    -- ** Response constructor
    , listPolicyVersionsResponse
    -- ** Response lenses
    , lpvrsVersions
    , lpvrsMarker
    , lpvrsIsTruncated
    , lpvrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listPolicyVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvrqMaxItems'
--
-- * 'lpvrqMarker'
--
-- * 'lpvrqPolicyARN'
data ListPolicyVersions = ListPolicyVersions'
    { _lpvrqMaxItems  :: !(Maybe Nat)
    , _lpvrqMarker    :: !(Maybe Text)
    , _lpvrqPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPolicyVersions' smart constructor.
listPolicyVersions :: Text -> ListPolicyVersions
listPolicyVersions pPolicyARN_ =
    ListPolicyVersions'
    { _lpvrqMaxItems = Nothing
    , _lpvrqMarker = Nothing
    , _lpvrqPolicyARN = pPolicyARN_
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lpvrqMaxItems :: Lens' ListPolicyVersions (Maybe Natural)
lpvrqMaxItems = lens _lpvrqMaxItems (\ s a -> s{_lpvrqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lpvrqMarker :: Lens' ListPolicyVersions (Maybe Text)
lpvrqMarker = lens _lpvrqMarker (\ s a -> s{_lpvrqMarker = a});

-- | FIXME: Undocumented member.
lpvrqPolicyARN :: Lens' ListPolicyVersions Text
lpvrqPolicyARN = lens _lpvrqPolicyARN (\ s a -> s{_lpvrqPolicyARN = a});

instance AWSRequest ListPolicyVersions where
        type Sv ListPolicyVersions = IAM
        type Rs ListPolicyVersions =
             ListPolicyVersionsResponse
        request = post
        response
          = receiveXMLWrapper "ListPolicyVersionsResult"
              (\ s h x ->
                 ListPolicyVersionsResponse' <$>
                   (x .@? "Versions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListPolicyVersions where
        toHeaders = const mempty

instance ToPath ListPolicyVersions where
        toPath = const "/"

instance ToQuery ListPolicyVersions where
        toQuery ListPolicyVersions'{..}
          = mconcat
              ["Action" =: ("ListPolicyVersions" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _lpvrqMaxItems,
               "Marker" =: _lpvrqMarker,
               "PolicyArn" =: _lpvrqPolicyARN]

-- | Contains the response to a successful ListPolicyVersions request.
--
-- /See:/ 'listPolicyVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvrsVersions'
--
-- * 'lpvrsMarker'
--
-- * 'lpvrsIsTruncated'
--
-- * 'lpvrsStatus'
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
    { _lpvrsVersions    :: !(Maybe [PolicyVersion])
    , _lpvrsMarker      :: !(Maybe Text)
    , _lpvrsIsTruncated :: !(Maybe Bool)
    , _lpvrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPolicyVersionsResponse' smart constructor.
listPolicyVersionsResponse :: Int -> ListPolicyVersionsResponse
listPolicyVersionsResponse pStatus_ =
    ListPolicyVersionsResponse'
    { _lpvrsVersions = Nothing
    , _lpvrsMarker = Nothing
    , _lpvrsIsTruncated = Nothing
    , _lpvrsStatus = pStatus_
    }

-- | A list of policy versions.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
lpvrsVersions :: Lens' ListPolicyVersionsResponse [PolicyVersion]
lpvrsVersions = lens _lpvrsVersions (\ s a -> s{_lpvrsVersions = a}) . _Default;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lpvrsMarker :: Lens' ListPolicyVersionsResponse (Maybe Text)
lpvrsMarker = lens _lpvrsMarker (\ s a -> s{_lpvrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lpvrsIsTruncated :: Lens' ListPolicyVersionsResponse (Maybe Bool)
lpvrsIsTruncated = lens _lpvrsIsTruncated (\ s a -> s{_lpvrsIsTruncated = a});

-- | FIXME: Undocumented member.
lpvrsStatus :: Lens' ListPolicyVersionsResponse Int
lpvrsStatus = lens _lpvrsStatus (\ s a -> s{_lpvrsStatus = a});
