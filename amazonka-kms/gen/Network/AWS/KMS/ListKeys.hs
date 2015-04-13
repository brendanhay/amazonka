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

-- Module      : Network.AWS.KMS.ListKeys
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

-- | Lists the customer master keys.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeys.html>
module Network.AWS.KMS.ListKeys
    (
    -- * Request
      ListKeys
    -- ** Request constructor
    , listKeys
    -- ** Request lenses
    , lkLimit
    , lkMarker

    -- * Response
    , ListKeysResponse
    -- ** Response constructor
    , listKeysResponse
    -- ** Response lenses
    , lkrKeys
    , lkrNextMarker
    , lkrTruncated
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data ListKeys = ListKeys
    { _lkLimit  :: Maybe Nat
    , _lkMarker :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListKeys' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkLimit' @::@ 'Maybe' 'Natural'
--
-- * 'lkMarker' @::@ 'Maybe' 'Text'
--
listKeys :: ListKeys
listKeys = ListKeys
    { _lkLimit  = Nothing
    , _lkMarker = Nothing
    }

-- | Specify this parameter only when paginating results to indicate the maximum
-- number of keys you want listed in the response. If there are additional keys
-- beyond the maximum you specify, the 'Truncated' response element will be set to 'true.'
lkLimit :: Lens' ListKeys (Maybe Natural)
lkLimit = lens _lkLimit (\s a -> s { _lkLimit = a }) . mapping _Nat

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated. Set
-- it to the value of the 'NextMarker' in the response you just received.
lkMarker :: Lens' ListKeys (Maybe Text)
lkMarker = lens _lkMarker (\s a -> s { _lkMarker = a })

data ListKeysResponse = ListKeysResponse
    { _lkrKeys       :: List "Keys" KeyListEntry
    , _lkrNextMarker :: Maybe Text
    , _lkrTruncated  :: Maybe Bool
    } deriving (Eq, Read, Show)

-- | 'ListKeysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkrKeys' @::@ ['KeyListEntry']
--
-- * 'lkrNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'lkrTruncated' @::@ 'Maybe' 'Bool'
--
listKeysResponse :: ListKeysResponse
listKeysResponse = ListKeysResponse
    { _lkrKeys       = mempty
    , _lkrNextMarker = Nothing
    , _lkrTruncated  = Nothing
    }

-- | A list of keys.
lkrKeys :: Lens' ListKeysResponse [KeyListEntry]
lkrKeys = lens _lkrKeys (\s a -> s { _lkrKeys = a }) . _List

-- | If 'Truncated' is true, this value is present and contains the value to use for
-- the 'Marker' request parameter in a subsequent pagination request.
lkrNextMarker :: Lens' ListKeysResponse (Maybe Text)
lkrNextMarker = lens _lkrNextMarker (\s a -> s { _lkrNextMarker = a })

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the 'Marker' request parameter to retrieve more keys in the list.
lkrTruncated :: Lens' ListKeysResponse (Maybe Bool)
lkrTruncated = lens _lkrTruncated (\s a -> s { _lkrTruncated = a })

instance ToPath ListKeys where
    toPath = const "/"

instance ToQuery ListKeys where
    toQuery = const mempty

instance ToHeaders ListKeys

instance ToJSON ListKeys where
    toJSON ListKeys{..} = object
        [ "Limit"  .= _lkLimit
        , "Marker" .= _lkMarker
        ]

instance AWSRequest ListKeys where
    type Sv ListKeys = KMS
    type Rs ListKeys = ListKeysResponse

    request  = post "ListKeys"
    response = jsonResponse

instance FromJSON ListKeysResponse where
    parseJSON = withObject "ListKeysResponse" $ \o -> ListKeysResponse
        <$> o .:? "Keys" .!= mempty
        <*> o .:? "NextMarker"
        <*> o .:? "Truncated"
