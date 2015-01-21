{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- you can still paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- If the 'UserName' field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request. Because
-- this action works for access keys under the AWS account, you can use this
-- action to manage root credentials even if the AWS account has no associated
-- users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSigningCertificates.html>
module Network.AWS.IAM.ListSigningCertificates
    (
    -- * Request
      ListSigningCertificates
    -- ** Request constructor
    , listSigningCertificates
    -- ** Request lenses
    , lsc1Marker
    , lsc1MaxItems
    , lsc1UserName

    -- * Response
    , ListSigningCertificatesResponse
    -- ** Response constructor
    , listSigningCertificatesResponse
    -- ** Response lenses
    , lscr1Certificates
    , lscr1IsTruncated
    , lscr1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListSigningCertificates = ListSigningCertificates
    { _lsc1Marker   :: Maybe Text
    , _lsc1MaxItems :: Maybe Nat
    , _lsc1UserName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListSigningCertificates' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsc1Marker' @::@ 'Maybe' 'Text'
--
-- * 'lsc1MaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lsc1UserName' @::@ 'Maybe' 'Text'
--
listSigningCertificates :: ListSigningCertificates
listSigningCertificates = ListSigningCertificates
    { _lsc1UserName = Nothing
    , _lsc1Marker   = Nothing
    , _lsc1MaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the 'Marker' element in the response you just received.
lsc1Marker :: Lens' ListSigningCertificates (Maybe Text)
lsc1Marker = lens _lsc1Marker (\s a -> s { _lsc1Marker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- certificate IDs you want in the response. If there are additional certificate
-- IDs beyond the maximum you specify, the 'IsTruncated' response element is 'true'.
-- This parameter is optional. If you do not include it, it defaults to 100.
lsc1MaxItems :: Lens' ListSigningCertificates (Maybe Natural)
lsc1MaxItems = lens _lsc1MaxItems (\s a -> s { _lsc1MaxItems = a }) . mapping _Nat

-- | The name of the user.
lsc1UserName :: Lens' ListSigningCertificates (Maybe Text)
lsc1UserName = lens _lsc1UserName (\s a -> s { _lsc1UserName = a })

data ListSigningCertificatesResponse = ListSigningCertificatesResponse
    { _lscr1Certificates :: List "member" SigningCertificate
    , _lscr1IsTruncated  :: Maybe Bool
    , _lscr1Marker       :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListSigningCertificatesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscr1Certificates' @::@ ['SigningCertificate']
--
-- * 'lscr1IsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lscr1Marker' @::@ 'Maybe' 'Text'
--
listSigningCertificatesResponse :: ListSigningCertificatesResponse
listSigningCertificatesResponse = ListSigningCertificatesResponse
    { _lscr1Certificates = mempty
    , _lscr1IsTruncated  = Nothing
    , _lscr1Marker       = Nothing
    }

-- | A list of the user's signing certificate information.
lscr1Certificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lscr1Certificates =
    lens _lscr1Certificates (\s a -> s { _lscr1Certificates = a })
        . _List

-- | A flag that indicates whether there are more certificate IDs to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more certificates in the list.
lscr1IsTruncated :: Lens' ListSigningCertificatesResponse (Maybe Bool)
lscr1IsTruncated = lens _lscr1IsTruncated (\s a -> s { _lscr1IsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to
-- use for the 'Marker' parameter in a subsequent pagination request.
lscr1Marker :: Lens' ListSigningCertificatesResponse (Maybe Text)
lscr1Marker = lens _lscr1Marker (\s a -> s { _lscr1Marker = a })

instance ToPath ListSigningCertificates where
    toPath = const "/"

instance ToQuery ListSigningCertificates where
    toQuery ListSigningCertificates{..} = mconcat
        [ "Marker"   =? _lsc1Marker
        , "MaxItems" =? _lsc1MaxItems
        , "UserName" =? _lsc1UserName
        ]

instance ToHeaders ListSigningCertificates

instance AWSRequest ListSigningCertificates where
    type Sv ListSigningCertificates = IAM
    type Rs ListSigningCertificates = ListSigningCertificatesResponse

    request  = post "ListSigningCertificates"
    response = xmlResponse

instance FromXML ListSigningCertificatesResponse where
    parseXML = withElement "ListSigningCertificatesResult" $ \x -> ListSigningCertificatesResponse
        <$> x .@? "Certificates" .!@ mempty
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"

instance AWSPager ListSigningCertificates where
    page rq rs
        | stop (rs ^. lscr1IsTruncated) = Nothing
        | otherwise = Just $ rq
            & lsc1Marker .~ rs ^. lscr1Marker
