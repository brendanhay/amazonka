{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListMFADevices
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the MFA devices. If the request includes the user name, then this
-- action lists all the MFA devices associated with the specified user
-- name. If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListMFADevices.html>
module Network.AWS.IAM.ListMFADevices
    (
    -- * Request
      ListMFADevices
    -- ** Request constructor
    , listMFADevices
    -- ** Request lenses
    , lmdUserName
    , lmdMaxItems
    , lmdMarker

    -- * Response
    , ListMFADevicesResponse
    -- ** Response constructor
    , listMFADevicesResponse
    -- ** Response lenses
    , lmdrMarker
    , lmdrIsTruncated
    , lmdrStatus
    , lmdrMFADevices
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listMFADevices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmdUserName'
--
-- * 'lmdMaxItems'
--
-- * 'lmdMarker'
data ListMFADevices = ListMFADevices'
    { _lmdUserName :: !(Maybe Text)
    , _lmdMaxItems :: !(Maybe Nat)
    , _lmdMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListMFADevices' smart constructor.
listMFADevices :: ListMFADevices
listMFADevices =
    ListMFADevices'
    { _lmdUserName = Nothing
    , _lmdMaxItems = Nothing
    , _lmdMarker = Nothing
    }

-- | The name of the user whose MFA devices you want to list.
lmdUserName :: Lens' ListMFADevices (Maybe Text)
lmdUserName = lens _lmdUserName (\ s a -> s{_lmdUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- MFA devices you want in the response. If there are additional MFA
-- devices beyond the maximum you specify, the @IsTruncated@ response
-- element is @true@. This parameter is optional. If you do not include it,
-- it defaults to 100.
lmdMaxItems :: Lens' ListMFADevices (Maybe Natural)
lmdMaxItems = lens _lmdMaxItems (\ s a -> s{_lmdMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lmdMarker :: Lens' ListMFADevices (Maybe Text)
lmdMarker = lens _lmdMarker (\ s a -> s{_lmdMarker = a});

instance AWSPager ListMFADevices where
        page rq rs
          | stop (rs ^. lmdrIsTruncated) = Nothing
          | isNothing (rs ^. lmdrMarker) = Nothing
          | otherwise =
            Just $ rq & lmdMarker .~ rs ^. lmdrMarker

instance AWSRequest ListMFADevices where
        type Sv ListMFADevices = IAM
        type Rs ListMFADevices = ListMFADevicesResponse
        request = post
        response
          = receiveXMLWrapper "ListMFADevicesResult"
              (\ s h x ->
                 ListMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure s)
                     <*>
                     (x .@? "MFADevices" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListMFADevices where
        toHeaders = const mempty

instance ToPath ListMFADevices where
        toPath = const "/"

instance ToQuery ListMFADevices where
        toQuery ListMFADevices'{..}
          = mconcat
              ["Action" =: ("ListMFADevices" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lmdUserName,
               "MaxItems" =: _lmdMaxItems, "Marker" =: _lmdMarker]

-- | Contains the response to a successful ListMFADevices request.
--
-- /See:/ 'listMFADevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmdrMarker'
--
-- * 'lmdrIsTruncated'
--
-- * 'lmdrStatus'
--
-- * 'lmdrMFADevices'
data ListMFADevicesResponse = ListMFADevicesResponse'
    { _lmdrMarker      :: !(Maybe Text)
    , _lmdrIsTruncated :: !(Maybe Bool)
    , _lmdrStatus      :: !Status
    , _lmdrMFADevices  :: ![MFADevice]
    } deriving (Eq,Show)

-- | 'ListMFADevicesResponse' smart constructor.
listMFADevicesResponse :: Status -> ListMFADevicesResponse
listMFADevicesResponse pStatus =
    ListMFADevicesResponse'
    { _lmdrMarker = Nothing
    , _lmdrIsTruncated = Nothing
    , _lmdrStatus = pStatus
    , _lmdrMFADevices = mempty
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lmdrMarker :: Lens' ListMFADevicesResponse (Maybe Text)
lmdrMarker = lens _lmdrMarker (\ s a -> s{_lmdrMarker = a});

-- | A flag that indicates whether there are more MFA devices to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more MFA
-- devices in the list.
lmdrIsTruncated :: Lens' ListMFADevicesResponse (Maybe Bool)
lmdrIsTruncated = lens _lmdrIsTruncated (\ s a -> s{_lmdrIsTruncated = a});

-- | FIXME: Undocumented member.
lmdrStatus :: Lens' ListMFADevicesResponse Status
lmdrStatus = lens _lmdrStatus (\ s a -> s{_lmdrStatus = a});

-- | A list of MFA devices.
lmdrMFADevices :: Lens' ListMFADevicesResponse [MFADevice]
lmdrMFADevices = lens _lmdrMFADevices (\ s a -> s{_lmdrMFADevices = a});
