{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the signing certificates associated with the
-- specified user. If there are none, the action returns an empty list.
--
-- Although each user is limited to a small number of signing certificates,
-- you can still paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- If the @UserName@ field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSigningCertificates.html>
module Network.AWS.IAM.ListSigningCertificates
    (
    -- * Request
      ListSigningCertificates
    -- ** Request constructor
    , listSigningCertificates
    -- ** Request lenses
    , lUserName
    , lMaxItems
    , lMarker

    -- * Response
    , ListSigningCertificatesResponse
    -- ** Response constructor
    , listSigningCertificatesResponse
    -- ** Response lenses
    , lisMarker
    , lisIsTruncated
    , lisStatus
    , lisCertificates
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listSigningCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lUserName'
--
-- * 'lMaxItems'
--
-- * 'lMarker'
data ListSigningCertificates = ListSigningCertificates'
    { _lUserName :: !(Maybe Text)
    , _lMaxItems :: !(Maybe Nat)
    , _lMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListSigningCertificates' smart constructor.
listSigningCertificates :: ListSigningCertificates
listSigningCertificates =
    ListSigningCertificates'
    { _lUserName = Nothing
    , _lMaxItems = Nothing
    , _lMarker = Nothing
    }

-- | The name of the user.
lUserName :: Lens' ListSigningCertificates (Maybe Text)
lUserName = lens _lUserName (\ s a -> s{_lUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- certificate IDs you want in the response. If there are additional
-- certificate IDs beyond the maximum you specify, the @IsTruncated@
-- response element is @true@. This parameter is optional. If you do not
-- include it, it defaults to 100.
lMaxItems :: Lens' ListSigningCertificates (Maybe Natural)
lMaxItems = lens _lMaxItems (\ s a -> s{_lMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lMarker :: Lens' ListSigningCertificates (Maybe Text)
lMarker = lens _lMarker (\ s a -> s{_lMarker = a});

instance AWSPager ListSigningCertificates where
        page rq rs
          | stop (rs ^. lisIsTruncated) = Nothing
          | isNothing (rs ^. lisMarker) = Nothing
          | otherwise = Just $ rq & lMarker .~ rs ^. lisMarker

instance AWSRequest ListSigningCertificates where
        type Sv ListSigningCertificates = IAM
        type Rs ListSigningCertificates =
             ListSigningCertificatesResponse
        request = post
        response
          = receiveXMLWrapper "ListSigningCertificatesResult"
              (\ s h x ->
                 ListSigningCertificatesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Certificates" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListSigningCertificates where
        toHeaders = const mempty

instance ToPath ListSigningCertificates where
        toPath = const "/"

instance ToQuery ListSigningCertificates where
        toQuery ListSigningCertificates'{..}
          = mconcat
              ["Action" =:
                 ("ListSigningCertificates" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lUserName, "MaxItems" =: _lMaxItems,
               "Marker" =: _lMarker]

-- | Contains the response to a successful ListSigningCertificates request.
--
-- /See:/ 'listSigningCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisMarker'
--
-- * 'lisIsTruncated'
--
-- * 'lisStatus'
--
-- * 'lisCertificates'
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'
    { _lisMarker       :: !(Maybe Text)
    , _lisIsTruncated  :: !(Maybe Bool)
    , _lisStatus       :: !Int
    , _lisCertificates :: ![SigningCertificate]
    } deriving (Eq,Read,Show)

-- | 'ListSigningCertificatesResponse' smart constructor.
listSigningCertificatesResponse :: Int -> ListSigningCertificatesResponse
listSigningCertificatesResponse pStatus =
    ListSigningCertificatesResponse'
    { _lisMarker = Nothing
    , _lisIsTruncated = Nothing
    , _lisStatus = pStatus
    , _lisCertificates = mempty
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lisMarker :: Lens' ListSigningCertificatesResponse (Maybe Text)
lisMarker = lens _lisMarker (\ s a -> s{_lisMarker = a});

-- | A flag that indicates whether there are more certificate IDs to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more
-- certificates in the list.
lisIsTruncated :: Lens' ListSigningCertificatesResponse (Maybe Bool)
lisIsTruncated = lens _lisIsTruncated (\ s a -> s{_lisIsTruncated = a});

-- | FIXME: Undocumented member.
lisStatus :: Lens' ListSigningCertificatesResponse Int
lisStatus = lens _lisStatus (\ s a -> s{_lisStatus = a});

-- | A list of the user\'s signing certificate information.
lisCertificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lisCertificates = lens _lisCertificates (\ s a -> s{_lisCertificates = a});
