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

-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an
-- Alias for Your AWS Account ID> in the /Using IAM/ guide. You can paginate
-- the results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliases.html>
module Network.AWS.IAM.ListAccountAliases
    (
    -- * Request
      ListAccountAliases
    -- ** Request constructor
    , listAccountAliases
    -- ** Request lenses
    , laaMarker
    , laaMaxItems

    -- * Response
    , ListAccountAliasesResponse
    -- ** Response constructor
    , listAccountAliasesResponse
    -- ** Response lenses
    , laarAccountAliases
    , laarIsTruncated
    , laarMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListAccountAliases = ListAccountAliases
    { _laaMarker   :: Maybe Text
    , _laaMaxItems :: Maybe Nat
    } deriving (Eq, Ord, Show)

-- | 'ListAccountAliases' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laaMarker' @::@ 'Maybe' 'Text'
--
-- * 'laaMaxItems' @::@ 'Maybe' 'Natural'
--
listAccountAliases :: ListAccountAliases
listAccountAliases = ListAccountAliases
    { _laaMarker   = Nothing
    , _laaMaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
laaMarker :: Lens' ListAccountAliases (Maybe Text)
laaMarker = lens _laaMarker (\s a -> s { _laaMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- account aliases you want in the response. If there are additional account
-- aliases beyond the maximum you specify, the IsTruncated response element
-- is true. This parameter is optional. If you do not include it, it
-- defaults to 100.
laaMaxItems :: Lens' ListAccountAliases (Maybe Natural)
laaMaxItems = lens _laaMaxItems (\s a -> s { _laaMaxItems = a }) . mapping _Nat

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { _laarAccountAliases :: List "AccountAliases" Text
    , _laarIsTruncated    :: Maybe Bool
    , _laarMarker         :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListAccountAliasesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laarAccountAliases' @::@ ['Text']
--
-- * 'laarIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'laarMarker' @::@ 'Maybe' 'Text'
--
listAccountAliasesResponse :: ListAccountAliasesResponse
listAccountAliasesResponse = ListAccountAliasesResponse
    { _laarAccountAliases = mempty
    , _laarIsTruncated    = Nothing
    , _laarMarker         = Nothing
    }

-- | A list of aliases associated with the account.
laarAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarAccountAliases =
    lens _laarAccountAliases (\s a -> s { _laarAccountAliases = a })
        . _List

-- | A flag that indicates whether there are more account aliases to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more account aliases in
-- the list.
laarIsTruncated :: Lens' ListAccountAliasesResponse (Maybe Bool)
laarIsTruncated = lens _laarIsTruncated (\s a -> s { _laarIsTruncated = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
laarMarker :: Lens' ListAccountAliasesResponse (Maybe Text)
laarMarker = lens _laarMarker (\s a -> s { _laarMarker = a })

instance ToPath ListAccountAliases where
    toPath = const "/"

instance ToQuery ListAccountAliases where
    toQuery ListAccountAliases{..} = mconcat
        [ "Marker"   =? _laaMarker
        , "MaxItems" =? _laaMaxItems
        ]

instance ToHeaders ListAccountAliases

instance AWSRequest ListAccountAliases where
    type Sv ListAccountAliases = IAM
    type Rs ListAccountAliases = ListAccountAliasesResponse

    request  = post "ListAccountAliases"
    response = xmlResponse

instance FromXML ListAccountAliasesResponse where
    parseXML = withElement "ListAccountAliasesResult" $ \x -> ListAccountAliasesResponse
        <$> x .@  "AccountAliases"
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"

instance AWSPager ListAccountAliases where
    page rq rs
        | stop (rs ^. laarIsTruncated) = Nothing
        | otherwise = Just $ rq
            & laaMarker .~ rs ^. laarMarker
