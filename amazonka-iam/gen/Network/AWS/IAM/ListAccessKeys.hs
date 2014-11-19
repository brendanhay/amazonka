{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the access key IDs associated with the specified
-- user. If there are none, the action returns an empty list. Although each
-- user is limited to a small number of keys, you can still paginate the
-- results using the MaxItems and Marker parameters. If the UserName field is
-- not specified, the UserName is determined implicitly based on the AWS
-- access key ID used to sign the request. Because this action works for
-- access keys under the AWS account, you can use this action to manage root
-- credentials even if the AWS account has no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeys.html>
module Network.AWS.IAM.ListAccessKeys
    (
    -- * Request
      ListAccessKeys
    -- ** Request constructor
    , listAccessKeys
    -- ** Request lenses
    , lakMarker
    , lakMaxItems
    , lakUserName

    -- * Response
    , ListAccessKeysResponse
    -- ** Response constructor
    , listAccessKeysResponse
    -- ** Response lenses
    , lakrAccessKeyMetadata
    , lakrIsTruncated
    , lakrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListAccessKeys = ListAccessKeys
    { _lakMarker   :: Maybe Text
    , _lakMaxItems :: Maybe Nat
    , _lakUserName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListAccessKeys' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lakMarker' @::@ 'Maybe' 'Text'
--
-- * 'lakMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lakUserName' @::@ 'Maybe' 'Text'
--
listAccessKeys :: ListAccessKeys
listAccessKeys = ListAccessKeys
    { _lakUserName = Nothing
    , _lakMarker   = Nothing
    , _lakMaxItems = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lakMarker :: Lens' ListAccessKeys (Maybe Text)
lakMarker = lens _lakMarker (\s a -> s { _lakMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of keys you want in the response. If there are additional keys
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lakMaxItems :: Lens' ListAccessKeys (Maybe Natural)
lakMaxItems = lens _lakMaxItems (\s a -> s { _lakMaxItems = a })
    . mapping _Nat

-- | The name of the user.
lakUserName :: Lens' ListAccessKeys (Maybe Text)
lakUserName = lens _lakUserName (\s a -> s { _lakUserName = a })

data ListAccessKeysResponse = ListAccessKeysResponse
    { _lakrAccessKeyMetadata :: [AccessKeyMetadata]
    , _lakrIsTruncated       :: Maybe Bool
    , _lakrMarker            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListAccessKeysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lakrAccessKeyMetadata' @::@ ['AccessKeyMetadata']
--
-- * 'lakrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lakrMarker' @::@ 'Maybe' 'Text'
--
listAccessKeysResponse :: ListAccessKeysResponse
listAccessKeysResponse = ListAccessKeysResponse
    { _lakrAccessKeyMetadata = mempty
    , _lakrIsTruncated       = Nothing
    , _lakrMarker            = Nothing
    }

-- | A list of access key metadata.
lakrAccessKeyMetadata :: Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrAccessKeyMetadata =
    lens _lakrAccessKeyMetadata (\s a -> s { _lakrAccessKeyMetadata = a })

-- | A flag that indicates whether there are more keys to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more keys in the list.
lakrIsTruncated :: Lens' ListAccessKeysResponse (Maybe Bool)
lakrIsTruncated = lens _lakrIsTruncated (\s a -> s { _lakrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lakrMarker :: Lens' ListAccessKeysResponse (Maybe Text)
lakrMarker = lens _lakrMarker (\s a -> s { _lakrMarker = a })

instance ToPath ListAccessKeys where
    toPath = const "/"

instance ToQuery ListAccessKeys

instance ToHeaders ListAccessKeys

instance AWSRequest ListAccessKeys where
    type Sv ListAccessKeys = IAM
    type Rs ListAccessKeys = ListAccessKeysResponse

    request  = post "ListAccessKeys"
    response = xmlResponse

instance FromXML ListAccessKeysResponse where
    parseXML = withElement "ListAccessKeysResult" $ \x ->
        ListAccessKeysResponse
            <$> x .@ "AccessKeyMetadata"
            <*> x .@? "IsTruncated"
            <*> x .@? "Marker"
