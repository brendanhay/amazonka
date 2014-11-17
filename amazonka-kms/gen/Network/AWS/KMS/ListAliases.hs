{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all of the key aliases in the account.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListAliases.html>
module Network.AWS.KMS.ListAliases
    (
    -- * Request
      ListAliases
    -- ** Request constructor
    , listAliases
    -- ** Request lenses
    , laLimit
    , laMarker

    -- * Response
    , ListAliasesResponse
    -- ** Response constructor
    , listAliasesResponse
    -- ** Response lenses
    , larAliases
    , larNextMarker
    , larTruncated
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data ListAliases = ListAliases
    { _laLimit  :: Maybe Nat
    , _laMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListAliases' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laLimit' @::@ 'Maybe' 'Natural'
--
-- * 'laMarker' @::@ 'Maybe' 'Text'
--
listAliases :: ListAliases
listAliases = ListAliases
    { _laLimit  = Nothing
    , _laMarker = Nothing
    }

-- | Specify this parameter when paginating results to indicate the maximum
-- number of aliases you want in each response. If there are additional
-- aliases beyond the maximum you specify, the Truncated response element
-- will be set to true.
laLimit :: Lens' ListAliases (Maybe Natural)
laLimit = lens _laLimit (\s a -> s { _laLimit = a })
    . mapping _Nat

-- | Use this parameter when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the NextMarker element in the response you just
-- received.
laMarker :: Lens' ListAliases (Maybe Text)
laMarker = lens _laMarker (\s a -> s { _laMarker = a })

data ListAliasesResponse = ListAliasesResponse
    { _larAliases    :: [AliasListEntry]
    , _larNextMarker :: Maybe Text
    , _larTruncated  :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ListAliasesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larAliases' @::@ ['AliasListEntry']
--
-- * 'larNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'larTruncated' @::@ 'Maybe' 'Bool'
--
listAliasesResponse :: ListAliasesResponse
listAliasesResponse = ListAliasesResponse
    { _larAliases    = mempty
    , _larNextMarker = Nothing
    , _larTruncated  = Nothing
    }

-- | A list of key aliases in the user's account.
larAliases :: Lens' ListAliasesResponse [AliasListEntry]
larAliases = lens _larAliases (\s a -> s { _larAliases = a })

-- | If Truncated is true, this value is present and contains the value to use
-- for the Marker request parameter in a subsequent pagination request.
larNextMarker :: Lens' ListAliasesResponse (Maybe Text)
larNextMarker = lens _larNextMarker (\s a -> s { _larNextMarker = a })

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more aliases in the list.
larTruncated :: Lens' ListAliasesResponse (Maybe Bool)
larTruncated = lens _larTruncated (\s a -> s { _larTruncated = a })

instance ToPath ListAliases where
    toPath = const "/"

instance ToQuery ListAliases where
    toQuery = const mempty

instance ToHeaders ListAliases
instance ToJSON ListAliases where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ListAliases where
    type Sv ListAliases = KMS
    type Rs ListAliases = ListAliasesResponse

    request  = post "ListAliases"
    response = jsonResponse

instance FromJSON ListAliasesResponse where
    parseJSON = genericParseJSON jsonOptions
