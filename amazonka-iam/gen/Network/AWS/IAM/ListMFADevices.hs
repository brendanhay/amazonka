{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListMFADevices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the MFA devices. If the request includes the user name, then this
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
    , lmdrqUserName
    , lmdrqMaxItems
    , lmdrqMarker

    -- * Response
    , ListMFADevicesResponse
    -- ** Response constructor
    , listMFADevicesResponse
    -- ** Response lenses
    , lmdrsMarker
    , lmdrsIsTruncated
    , lmdrsStatus
    , lmdrsMFADevices
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
-- * 'lmdrqUserName'
--
-- * 'lmdrqMaxItems'
--
-- * 'lmdrqMarker'
data ListMFADevices = ListMFADevices'
    { _lmdrqUserName :: !(Maybe Text)
    , _lmdrqMaxItems :: !(Maybe Nat)
    , _lmdrqMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListMFADevices' smart constructor.
listMFADevices :: ListMFADevices
listMFADevices =
    ListMFADevices'
    { _lmdrqUserName = Nothing
    , _lmdrqMaxItems = Nothing
    , _lmdrqMarker = Nothing
    }

-- | The name of the user whose MFA devices you want to list.
lmdrqUserName :: Lens' ListMFADevices (Maybe Text)
lmdrqUserName = lens _lmdrqUserName (\ s a -> s{_lmdrqUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lmdrqMaxItems :: Lens' ListMFADevices (Maybe Natural)
lmdrqMaxItems = lens _lmdrqMaxItems (\ s a -> s{_lmdrqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lmdrqMarker :: Lens' ListMFADevices (Maybe Text)
lmdrqMarker = lens _lmdrqMarker (\ s a -> s{_lmdrqMarker = a});

instance AWSPager ListMFADevices where
        page rq rs
          | stop (rs ^. lmdrsIsTruncated) = Nothing
          | isNothing (rs ^. lmdrsMarker) = Nothing
          | otherwise =
            Just $ rq & lmdrqMarker .~ rs ^. lmdrsMarker

instance AWSRequest ListMFADevices where
        type Sv ListMFADevices = IAM
        type Rs ListMFADevices = ListMFADevicesResponse
        request = post
        response
          = receiveXMLWrapper "ListMFADevicesResult"
              (\ s h x ->
                 ListMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
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
               "UserName" =: _lmdrqUserName,
               "MaxItems" =: _lmdrqMaxItems,
               "Marker" =: _lmdrqMarker]

-- | Contains the response to a successful ListMFADevices request.
--
-- /See:/ 'listMFADevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmdrsMarker'
--
-- * 'lmdrsIsTruncated'
--
-- * 'lmdrsStatus'
--
-- * 'lmdrsMFADevices'
data ListMFADevicesResponse = ListMFADevicesResponse'
    { _lmdrsMarker      :: !(Maybe Text)
    , _lmdrsIsTruncated :: !(Maybe Bool)
    , _lmdrsStatus      :: !Int
    , _lmdrsMFADevices  :: ![MFADevice]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListMFADevicesResponse' smart constructor.
listMFADevicesResponse :: Int -> ListMFADevicesResponse
listMFADevicesResponse pStatus =
    ListMFADevicesResponse'
    { _lmdrsMarker = Nothing
    , _lmdrsIsTruncated = Nothing
    , _lmdrsStatus = pStatus
    , _lmdrsMFADevices = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lmdrsMarker :: Lens' ListMFADevicesResponse (Maybe Text)
lmdrsMarker = lens _lmdrsMarker (\ s a -> s{_lmdrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lmdrsIsTruncated :: Lens' ListMFADevicesResponse (Maybe Bool)
lmdrsIsTruncated = lens _lmdrsIsTruncated (\ s a -> s{_lmdrsIsTruncated = a});

-- | FIXME: Undocumented member.
lmdrsStatus :: Lens' ListMFADevicesResponse Int
lmdrsStatus = lens _lmdrsStatus (\ s a -> s{_lmdrsStatus = a});

-- | A list of MFA devices.
lmdrsMFADevices :: Lens' ListMFADevicesResponse [MFADevice]
lmdrsMFADevices = lens _lmdrsMFADevices (\ s a -> s{_lmdrsMFADevices = a});
