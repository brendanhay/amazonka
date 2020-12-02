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
-- Module      : Network.AWS.IAM.ListMFADevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the MFA devices for an IAM user. If the request includes a IAM user name, then this operation lists all the MFA devices associated with the specified user. If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request for this API.
--
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListMFADevices
    (
    -- * Creating a Request
      listMFADevices
    , ListMFADevices
    -- * Request Lenses
    , lmdUserName
    , lmdMarker
    , lmdMaxItems

    -- * Destructuring the Response
    , listMFADevicesResponse
    , ListMFADevicesResponse
    -- * Response Lenses
    , lmdrsMarker
    , lmdrsIsTruncated
    , lmdrsResponseStatus
    , lmdrsMFADevices
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMFADevices' smart constructor.
data ListMFADevices = ListMFADevices'
  { _lmdUserName :: !(Maybe Text)
  , _lmdMarker   :: !(Maybe Text)
  , _lmdMaxItems :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMFADevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmdUserName' - The name of the user whose MFA devices you want to list. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'lmdMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lmdMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listMFADevices
    :: ListMFADevices
listMFADevices =
  ListMFADevices'
    {_lmdUserName = Nothing, _lmdMarker = Nothing, _lmdMaxItems = Nothing}


-- | The name of the user whose MFA devices you want to list. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lmdUserName :: Lens' ListMFADevices (Maybe Text)
lmdUserName = lens _lmdUserName (\ s a -> s{_lmdUserName = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lmdMarker :: Lens' ListMFADevices (Maybe Text)
lmdMarker = lens _lmdMarker (\ s a -> s{_lmdMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lmdMaxItems :: Lens' ListMFADevices (Maybe Natural)
lmdMaxItems = lens _lmdMaxItems (\ s a -> s{_lmdMaxItems = a}) . mapping _Nat

instance AWSPager ListMFADevices where
        page rq rs
          | stop (rs ^. lmdrsIsTruncated) = Nothing
          | isNothing (rs ^. lmdrsMarker) = Nothing
          | otherwise =
            Just $ rq & lmdMarker .~ rs ^. lmdrsMarker

instance AWSRequest ListMFADevices where
        type Rs ListMFADevices = ListMFADevicesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListMFADevicesResult"
              (\ s h x ->
                 ListMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "MFADevices" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListMFADevices where

instance NFData ListMFADevices where

instance ToHeaders ListMFADevices where
        toHeaders = const mempty

instance ToPath ListMFADevices where
        toPath = const "/"

instance ToQuery ListMFADevices where
        toQuery ListMFADevices'{..}
          = mconcat
              ["Action" =: ("ListMFADevices" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lmdUserName, "Marker" =: _lmdMarker,
               "MaxItems" =: _lmdMaxItems]

-- | Contains the response to a successful 'ListMFADevices' request.
--
--
--
-- /See:/ 'listMFADevicesResponse' smart constructor.
data ListMFADevicesResponse = ListMFADevicesResponse'
  { _lmdrsMarker         :: !(Maybe Text)
  , _lmdrsIsTruncated    :: !(Maybe Bool)
  , _lmdrsResponseStatus :: !Int
  , _lmdrsMFADevices     :: ![MFADevice]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMFADevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmdrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lmdrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lmdrsResponseStatus' - -- | The response status code.
--
-- * 'lmdrsMFADevices' - A list of MFA devices.
listMFADevicesResponse
    :: Int -- ^ 'lmdrsResponseStatus'
    -> ListMFADevicesResponse
listMFADevicesResponse pResponseStatus_ =
  ListMFADevicesResponse'
    { _lmdrsMarker = Nothing
    , _lmdrsIsTruncated = Nothing
    , _lmdrsResponseStatus = pResponseStatus_
    , _lmdrsMFADevices = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lmdrsMarker :: Lens' ListMFADevicesResponse (Maybe Text)
lmdrsMarker = lens _lmdrsMarker (\ s a -> s{_lmdrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lmdrsIsTruncated :: Lens' ListMFADevicesResponse (Maybe Bool)
lmdrsIsTruncated = lens _lmdrsIsTruncated (\ s a -> s{_lmdrsIsTruncated = a})

-- | -- | The response status code.
lmdrsResponseStatus :: Lens' ListMFADevicesResponse Int
lmdrsResponseStatus = lens _lmdrsResponseStatus (\ s a -> s{_lmdrsResponseStatus = a})

-- | A list of MFA devices.
lmdrsMFADevices :: Lens' ListMFADevicesResponse [MFADevice]
lmdrsMFADevices = lens _lmdrsMFADevices (\ s a -> s{_lmdrsMFADevices = a}) . _Coerce

instance NFData ListMFADevicesResponse where
