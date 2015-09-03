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
-- Module      : Network.AWS.IAM.ListAttachedRolePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified role.
--
-- A role can also have inline policies embedded with it. To list the
-- inline policies for a role, use the ListRolePolicies API. For
-- information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters. You can use the 'PathPrefix' parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified role (or none that match the
-- specified path prefix), the action returns an empty list.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedRolePolicies.html AWS API Reference> for ListAttachedRolePolicies.
module Network.AWS.IAM.ListAttachedRolePolicies
    (
    -- * Creating a Request
      listAttachedRolePolicies
    , ListAttachedRolePolicies
    -- * Request Lenses
    , larpPathPrefix
    , larpMarker
    , larpMaxItems
    , larpRoleName

    -- * Destructuring the Response
    , listAttachedRolePoliciesResponse
    , ListAttachedRolePoliciesResponse
    -- * Response Lenses
    , larprsAttachedPolicies
    , larprsMarker
    , larprsIsTruncated
    , larprsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAttachedRolePolicies' smart constructor.
data ListAttachedRolePolicies = ListAttachedRolePolicies'
    { _larpPathPrefix :: !(Maybe Text)
    , _larpMarker     :: !(Maybe Text)
    , _larpMaxItems   :: !(Maybe Nat)
    , _larpRoleName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAttachedRolePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larpPathPrefix'
--
-- * 'larpMarker'
--
-- * 'larpMaxItems'
--
-- * 'larpRoleName'
listAttachedRolePolicies
    :: Text -- ^ 'larpRoleName'
    -> ListAttachedRolePolicies
listAttachedRolePolicies pRoleName_ =
    ListAttachedRolePolicies'
    { _larpPathPrefix = Nothing
    , _larpMarker = Nothing
    , _larpMaxItems = Nothing
    , _larpRoleName = pRoleName_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
larpPathPrefix :: Lens' ListAttachedRolePolicies (Maybe Text)
larpPathPrefix = lens _larpPathPrefix (\ s a -> s{_larpPathPrefix = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
larpMarker :: Lens' ListAttachedRolePolicies (Maybe Text)
larpMarker = lens _larpMarker (\ s a -> s{_larpMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
larpMaxItems :: Lens' ListAttachedRolePolicies (Maybe Natural)
larpMaxItems = lens _larpMaxItems (\ s a -> s{_larpMaxItems = a}) . mapping _Nat;

-- | The name (friendly name, not ARN) of the role to list attached policies
-- for.
larpRoleName :: Lens' ListAttachedRolePolicies Text
larpRoleName = lens _larpRoleName (\ s a -> s{_larpRoleName = a});

instance AWSRequest ListAttachedRolePolicies where
        type Rs ListAttachedRolePolicies =
             ListAttachedRolePoliciesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListAttachedRolePoliciesResult"
              (\ s h x ->
                 ListAttachedRolePoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAttachedRolePolicies where
        toHeaders = const mempty

instance ToPath ListAttachedRolePolicies where
        toPath = const "/"

instance ToQuery ListAttachedRolePolicies where
        toQuery ListAttachedRolePolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _larpPathPrefix,
               "Marker" =: _larpMarker, "MaxItems" =: _larpMaxItems,
               "RoleName" =: _larpRoleName]

-- | Contains the response to a successful ListAttachedRolePolicies request.
--
-- /See:/ 'listAttachedRolePoliciesResponse' smart constructor.
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'
    { _larprsAttachedPolicies :: !(Maybe [AttachedPolicy])
    , _larprsMarker           :: !(Maybe Text)
    , _larprsIsTruncated      :: !(Maybe Bool)
    , _larprsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAttachedRolePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larprsAttachedPolicies'
--
-- * 'larprsMarker'
--
-- * 'larprsIsTruncated'
--
-- * 'larprsResponseStatus'
listAttachedRolePoliciesResponse
    :: Int -- ^ 'larprsResponseStatus'
    -> ListAttachedRolePoliciesResponse
listAttachedRolePoliciesResponse pResponseStatus_ =
    ListAttachedRolePoliciesResponse'
    { _larprsAttachedPolicies = Nothing
    , _larprsMarker = Nothing
    , _larprsIsTruncated = Nothing
    , _larprsResponseStatus = pResponseStatus_
    }

-- | A list of the attached policies.
larprsAttachedPolicies :: Lens' ListAttachedRolePoliciesResponse [AttachedPolicy]
larprsAttachedPolicies = lens _larprsAttachedPolicies (\ s a -> s{_larprsAttachedPolicies = a}) . _Default . _Coerce;

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
larprsMarker :: Lens' ListAttachedRolePoliciesResponse (Maybe Text)
larprsMarker = lens _larprsMarker (\ s a -> s{_larprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
larprsIsTruncated :: Lens' ListAttachedRolePoliciesResponse (Maybe Bool)
larprsIsTruncated = lens _larprsIsTruncated (\ s a -> s{_larprsIsTruncated = a});

-- | The response status code.
larprsResponseStatus :: Lens' ListAttachedRolePoliciesResponse Int
larprsResponseStatus = lens _larprsResponseStatus (\ s a -> s{_larprsResponseStatus = a});
