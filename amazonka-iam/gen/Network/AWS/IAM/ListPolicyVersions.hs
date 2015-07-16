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
    , lpvMaxItems
    , lpvMarker
    , lpvPolicyARN

    -- * Response
    , ListPolicyVersionsResponse
    -- ** Response constructor
    , listPolicyVersionsResponse
    -- ** Response lenses
    , lpvrVersions
    , lpvrMarker
    , lpvrIsTruncated
    , lpvrStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listPolicyVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvMaxItems'
--
-- * 'lpvMarker'
--
-- * 'lpvPolicyARN'
data ListPolicyVersions = ListPolicyVersions'
    { _lpvMaxItems  :: !(Maybe Nat)
    , _lpvMarker    :: !(Maybe Text)
    , _lpvPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPolicyVersions' smart constructor.
listPolicyVersions :: Text -> ListPolicyVersions
listPolicyVersions pPolicyARN =
    ListPolicyVersions'
    { _lpvMaxItems = Nothing
    , _lpvMarker = Nothing
    , _lpvPolicyARN = pPolicyARN
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lpvMaxItems :: Lens' ListPolicyVersions (Maybe Natural)
lpvMaxItems = lens _lpvMaxItems (\ s a -> s{_lpvMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lpvMarker :: Lens' ListPolicyVersions (Maybe Text)
lpvMarker = lens _lpvMarker (\ s a -> s{_lpvMarker = a});

-- | FIXME: Undocumented member.
lpvPolicyARN :: Lens' ListPolicyVersions Text
lpvPolicyARN = lens _lpvPolicyARN (\ s a -> s{_lpvPolicyARN = a});

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
               "MaxItems" =: _lpvMaxItems, "Marker" =: _lpvMarker,
               "PolicyArn" =: _lpvPolicyARN]

-- | Contains the response to a successful ListPolicyVersions request.
--
-- /See:/ 'listPolicyVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpvrVersions'
--
-- * 'lpvrMarker'
--
-- * 'lpvrIsTruncated'
--
-- * 'lpvrStatus'
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
    { _lpvrVersions    :: !(Maybe [PolicyVersion])
    , _lpvrMarker      :: !(Maybe Text)
    , _lpvrIsTruncated :: !(Maybe Bool)
    , _lpvrStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPolicyVersionsResponse' smart constructor.
listPolicyVersionsResponse :: Int -> ListPolicyVersionsResponse
listPolicyVersionsResponse pStatus =
    ListPolicyVersionsResponse'
    { _lpvrVersions = Nothing
    , _lpvrMarker = Nothing
    , _lpvrIsTruncated = Nothing
    , _lpvrStatus = pStatus
    }

-- | A list of policy versions.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
lpvrVersions :: Lens' ListPolicyVersionsResponse [PolicyVersion]
lpvrVersions = lens _lpvrVersions (\ s a -> s{_lpvrVersions = a}) . _Default;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lpvrMarker :: Lens' ListPolicyVersionsResponse (Maybe Text)
lpvrMarker = lens _lpvrMarker (\ s a -> s{_lpvrMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lpvrIsTruncated :: Lens' ListPolicyVersionsResponse (Maybe Bool)
lpvrIsTruncated = lens _lpvrIsTruncated (\ s a -> s{_lpvrIsTruncated = a});

-- | FIXME: Undocumented member.
lpvrStatus :: Lens' ListPolicyVersionsResponse Int
lpvrStatus = lens _lpvrStatus (\ s a -> s{_lpvrStatus = a});
