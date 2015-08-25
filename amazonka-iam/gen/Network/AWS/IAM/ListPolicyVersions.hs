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
-- Module      : Network.AWS.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the versions of the specified managed policy,
-- including the version that is set as the policy\'s default version.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicyVersions.html AWS API Reference> for ListPolicyVersions.
module Network.AWS.IAM.ListPolicyVersions
    (
    -- * Creating a Request
      listPolicyVersions
    , ListPolicyVersions
    -- * Request Lenses
    , lpvMaxItems
    , lpvMarker
    , lpvPolicyARN

    -- * Destructuring the Response
    , listPolicyVersionsResponse
    , ListPolicyVersionsResponse
    -- * Response Lenses
    , lpvrsVersions
    , lpvrsMarker
    , lpvrsIsTruncated
    , lpvrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
    { _lpvMaxItems  :: !(Maybe Nat)
    , _lpvMarker    :: !(Maybe Text)
    , _lpvPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPolicyVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvMaxItems'
--
-- * 'lpvMarker'
--
-- * 'lpvPolicyARN'
listPolicyVersions
    :: Text -- ^ 'lpvPolicyARN'
    -> ListPolicyVersions
listPolicyVersions pPolicyARN_ =
    ListPolicyVersions'
    { _lpvMaxItems = Nothing
    , _lpvMarker = Nothing
    , _lpvPolicyARN = pPolicyARN_
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lpvMaxItems :: Lens' ListPolicyVersions (Maybe Natural)
lpvMaxItems = lens _lpvMaxItems (\ s a -> s{_lpvMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lpvMarker :: Lens' ListPolicyVersions (Maybe Text)
lpvMarker = lens _lpvMarker (\ s a -> s{_lpvMarker = a});

-- | Undocumented member.
lpvPolicyARN :: Lens' ListPolicyVersions Text
lpvPolicyARN = lens _lpvPolicyARN (\ s a -> s{_lpvPolicyARN = a});

instance AWSRequest ListPolicyVersions where
        type Rs ListPolicyVersions =
             ListPolicyVersionsResponse
        request = postQuery iAM
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
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
    { _lpvrsVersions    :: !(Maybe [PolicyVersion])
    , _lpvrsMarker      :: !(Maybe Text)
    , _lpvrsIsTruncated :: !(Maybe Bool)
    , _lpvrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsVersions'
--
-- * 'lpvrsMarker'
--
-- * 'lpvrsIsTruncated'
--
-- * 'lpvrsStatus'
listPolicyVersionsResponse
    :: Int -- ^ 'lpvrsStatus'
    -> ListPolicyVersionsResponse
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
lpvrsVersions = lens _lpvrsVersions (\ s a -> s{_lpvrsVersions = a}) . _Default . _Coerce;

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lpvrsMarker :: Lens' ListPolicyVersionsResponse (Maybe Text)
lpvrsMarker = lens _lpvrsMarker (\ s a -> s{_lpvrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lpvrsIsTruncated :: Lens' ListPolicyVersionsResponse (Maybe Bool)
lpvrsIsTruncated = lens _lpvrsIsTruncated (\ s a -> s{_lpvrsIsTruncated = a});

-- | The response status code.
lpvrsStatus :: Lens' ListPolicyVersionsResponse Int
lpvrsStatus = lens _lpvrsStatus (\ s a -> s{_lpvrsStatus = a});
