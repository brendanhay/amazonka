{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListSigningCertificates
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
    , lisIsTruncated
    , lisCertificates
    , lisMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listSigningCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lUserName'
--
-- * 'lMaxItems'
--
-- * 'lMarker'
data ListSigningCertificates = ListSigningCertificates'{_lUserName :: Text, _lMaxItems :: Nat, _lMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListSigningCertificates' smart constructor.
listSigningCertificates :: Text -> Natural -> Text -> ListSigningCertificates
listSigningCertificates pUserName pMaxItems pMarker = ListSigningCertificates'{_lUserName = pUserName, _lMaxItems = _Nat # pMaxItems, _lMarker = pMarker};

-- | The name of the user.
lUserName :: Lens' ListSigningCertificates Text
lUserName = lens _lUserName (\ s a -> s{_lUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- certificate IDs you want in the response. If there are additional
-- certificate IDs beyond the maximum you specify, the @IsTruncated@
-- response element is @true@. This parameter is optional. If you do not
-- include it, it defaults to 100.
lMaxItems :: Lens' ListSigningCertificates Natural
lMaxItems = lens _lMaxItems (\ s a -> s{_lMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lMarker :: Lens' ListSigningCertificates Text
lMarker = lens _lMarker (\ s a -> s{_lMarker = a});

instance AWSRequest ListSigningCertificates where
        type Sv ListSigningCertificates = IAM
        type Rs ListSigningCertificates =
             ListSigningCertificatesResponse
        request = post
        response
          = receiveXMLWrapper "ListSigningCertificatesResult"
              (\ s h x ->
                 ListSigningCertificatesResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "Certificates" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

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

-- | /See:/ 'listSigningCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisIsTruncated'
--
-- * 'lisCertificates'
--
-- * 'lisMarker'
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'{_lisIsTruncated :: Maybe Bool, _lisCertificates :: [SigningCertificate], _lisMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListSigningCertificatesResponse' smart constructor.
listSigningCertificatesResponse :: [SigningCertificate] -> Text -> ListSigningCertificatesResponse
listSigningCertificatesResponse pCertificates pMarker = ListSigningCertificatesResponse'{_lisIsTruncated = Nothing, _lisCertificates = pCertificates, _lisMarker = pMarker};

-- | A flag that indicates whether there are more certificate IDs to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more
-- certificates in the list.
lisIsTruncated :: Lens' ListSigningCertificatesResponse (Maybe Bool)
lisIsTruncated = lens _lisIsTruncated (\ s a -> s{_lisIsTruncated = a});

-- | A list of the user\'s signing certificate information.
lisCertificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lisCertificates = lens _lisCertificates (\ s a -> s{_lisCertificates = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lisMarker :: Lens' ListSigningCertificatesResponse Text
lisMarker = lens _lisMarker (\ s a -> s{_lisMarker = a});
