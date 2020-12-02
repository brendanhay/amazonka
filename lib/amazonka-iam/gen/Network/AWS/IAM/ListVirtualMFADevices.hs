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
-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual MFA devices defined in the AWS account by assignment status. If you do not specify an assignment status, the operation returns a list of all virtual MFA devices. Assignment status can be @Assigned@ , @Unassigned@ , or @Any@ .
--
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListVirtualMFADevices
    (
    -- * Creating a Request
      listVirtualMFADevices
    , ListVirtualMFADevices
    -- * Request Lenses
    , lvmdAssignmentStatus
    , lvmdMarker
    , lvmdMaxItems

    -- * Destructuring the Response
    , listVirtualMFADevicesResponse
    , ListVirtualMFADevicesResponse
    -- * Response Lenses
    , lvmdrsMarker
    , lvmdrsIsTruncated
    , lvmdrsResponseStatus
    , lvmdrsVirtualMFADevices
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listVirtualMFADevices' smart constructor.
data ListVirtualMFADevices = ListVirtualMFADevices'
  { _lvmdAssignmentStatus :: !(Maybe AssignmentStatusType)
  , _lvmdMarker           :: !(Maybe Text)
  , _lvmdMaxItems         :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualMFADevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvmdAssignmentStatus' - The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ which lists both assigned and unassigned virtual MFA devices.
--
-- * 'lvmdMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lvmdMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listVirtualMFADevices
    :: ListVirtualMFADevices
listVirtualMFADevices =
  ListVirtualMFADevices'
    { _lvmdAssignmentStatus = Nothing
    , _lvmdMarker = Nothing
    , _lvmdMaxItems = Nothing
    }


-- | The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ which lists both assigned and unassigned virtual MFA devices.
lvmdAssignmentStatus :: Lens' ListVirtualMFADevices (Maybe AssignmentStatusType)
lvmdAssignmentStatus = lens _lvmdAssignmentStatus (\ s a -> s{_lvmdAssignmentStatus = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lvmdMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmdMarker = lens _lvmdMarker (\ s a -> s{_lvmdMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lvmdMaxItems :: Lens' ListVirtualMFADevices (Maybe Natural)
lvmdMaxItems = lens _lvmdMaxItems (\ s a -> s{_lvmdMaxItems = a}) . mapping _Nat

instance AWSPager ListVirtualMFADevices where
        page rq rs
          | stop (rs ^. lvmdrsIsTruncated) = Nothing
          | isNothing (rs ^. lvmdrsMarker) = Nothing
          | otherwise =
            Just $ rq & lvmdMarker .~ rs ^. lvmdrsMarker

instance AWSRequest ListVirtualMFADevices where
        type Rs ListVirtualMFADevices =
             ListVirtualMFADevicesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListVirtualMFADevicesResult"
              (\ s h x ->
                 ListVirtualMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "VirtualMFADevices" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListVirtualMFADevices where

instance NFData ListVirtualMFADevices where

instance ToHeaders ListVirtualMFADevices where
        toHeaders = const mempty

instance ToPath ListVirtualMFADevices where
        toPath = const "/"

instance ToQuery ListVirtualMFADevices where
        toQuery ListVirtualMFADevices'{..}
          = mconcat
              ["Action" =: ("ListVirtualMFADevices" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AssignmentStatus" =: _lvmdAssignmentStatus,
               "Marker" =: _lvmdMarker, "MaxItems" =: _lvmdMaxItems]

-- | Contains the response to a successful 'ListVirtualMFADevices' request.
--
--
--
-- /See:/ 'listVirtualMFADevicesResponse' smart constructor.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
  { _lvmdrsMarker            :: !(Maybe Text)
  , _lvmdrsIsTruncated       :: !(Maybe Bool)
  , _lvmdrsResponseStatus    :: !Int
  , _lvmdrsVirtualMFADevices :: ![VirtualMFADevice]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVirtualMFADevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvmdrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lvmdrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lvmdrsResponseStatus' - -- | The response status code.
--
-- * 'lvmdrsVirtualMFADevices' - The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
listVirtualMFADevicesResponse
    :: Int -- ^ 'lvmdrsResponseStatus'
    -> ListVirtualMFADevicesResponse
listVirtualMFADevicesResponse pResponseStatus_ =
  ListVirtualMFADevicesResponse'
    { _lvmdrsMarker = Nothing
    , _lvmdrsIsTruncated = Nothing
    , _lvmdrsResponseStatus = pResponseStatus_
    , _lvmdrsVirtualMFADevices = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lvmdrsMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmdrsMarker = lens _lvmdrsMarker (\ s a -> s{_lvmdrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lvmdrsIsTruncated :: Lens' ListVirtualMFADevicesResponse (Maybe Bool)
lvmdrsIsTruncated = lens _lvmdrsIsTruncated (\ s a -> s{_lvmdrsIsTruncated = a})

-- | -- | The response status code.
lvmdrsResponseStatus :: Lens' ListVirtualMFADevicesResponse Int
lvmdrsResponseStatus = lens _lvmdrsResponseStatus (\ s a -> s{_lvmdrsResponseStatus = a})

-- | The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
lvmdrsVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmdrsVirtualMFADevices = lens _lvmdrsVirtualMFADevices (\ s a -> s{_lvmdrsVirtualMFADevices = a}) . _Coerce

instance NFData ListVirtualMFADevicesResponse where
