{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the signing certificates associated with the
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
    , lrsMarker
    , lrsIsTruncated
    , lrsStatus
    , lrsCertificates
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lMaxItems :: Lens' ListSigningCertificates (Maybe Natural)
lMaxItems = lens _lMaxItems (\ s a -> s{_lMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lMarker :: Lens' ListSigningCertificates (Maybe Text)
lMarker = lens _lMarker (\ s a -> s{_lMarker = a});

instance AWSPager ListSigningCertificates where
        page rq rs
          | stop (rs ^. lrsIsTruncated) = Nothing
          | isNothing (rs ^. lrsMarker) = Nothing
          | otherwise = Just $ rq & lMarker .~ rs ^. lrsMarker

instance AWSRequest ListSigningCertificates where
        type Sv ListSigningCertificates = IAM
        type Rs ListSigningCertificates =
             ListSigningCertificatesResponse
        request = postQuery
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
        toPath = const mempty

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
-- * 'lrsMarker'
--
-- * 'lrsIsTruncated'
--
-- * 'lrsStatus'
--
-- * 'lrsCertificates'
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'
    { _lrsMarker       :: !(Maybe Text)
    , _lrsIsTruncated  :: !(Maybe Bool)
    , _lrsStatus       :: !Int
    , _lrsCertificates :: ![SigningCertificate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSigningCertificatesResponse' smart constructor.
listSigningCertificatesResponse :: Int -> ListSigningCertificatesResponse
listSigningCertificatesResponse pStatus_ =
    ListSigningCertificatesResponse'
    { _lrsMarker = Nothing
    , _lrsIsTruncated = Nothing
    , _lrsStatus = pStatus_
    , _lrsCertificates = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lrsMarker :: Lens' ListSigningCertificatesResponse (Maybe Text)
lrsMarker = lens _lrsMarker (\ s a -> s{_lrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lrsIsTruncated :: Lens' ListSigningCertificatesResponse (Maybe Bool)
lrsIsTruncated = lens _lrsIsTruncated (\ s a -> s{_lrsIsTruncated = a});

-- | FIXME: Undocumented member.
lrsStatus :: Lens' ListSigningCertificatesResponse Int
lrsStatus = lens _lrsStatus (\ s a -> s{_lrsStatus = a});

-- | A list of the user\'s signing certificate information.
lrsCertificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lrsCertificates = lens _lrsCertificates (\ s a -> s{_lrsCertificates = a}) . _Coerce;
