{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- about using an AWS account alias, see Using an Alias for Your AWS Account
-- ID in the Using IAM guide. You can paginate the results using the MaxItems
-- and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListAccountAliases &Version=2010-05-08 &AUTHPARAMS false
-- foocorporation c5a076e9-f1b0-11df-8fbe-45274EXAMPLE.
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

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListAccountAliases = ListAccountAliases
    { _laaMarker :: Maybe Text
    , _laaMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListAccountAliases' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listAccountAliases :: ListAccountAliases
listAccountAliases = ListAccountAliases
    { _laaMarker = Nothing
    , _laaMaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
laaMarker :: Lens' ListAccountAliases (Maybe Text)
laaMarker = lens _laaMarker (\s a -> s { _laaMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- account aliases you want in the response. If there are additional account
-- aliases beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
laaMaxItems :: Lens' ListAccountAliases (Maybe Integer)
laaMaxItems = lens _laaMaxItems (\s a -> s { _laaMaxItems = a })

instance ToQuery ListAccountAliases where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListAccountAliases
-- action.
data ListAccountAliasesResponse = ListAccountAliasesResponse
    { _laarAccountAliases :: [Text]
    , _laarIsTruncated :: !Bool
    , _laarMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListAccountAliasesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccountAliases ::@ @[Text]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
listAccountAliasesResponse :: [Text] -- ^ 'laarAccountAliases'
                           -> Bool -- ^ 'laarIsTruncated'
                           -> ListAccountAliasesResponse
listAccountAliasesResponse p1 p2 = ListAccountAliasesResponse
    { _laarAccountAliases = p1
    , _laarIsTruncated = p2
    , _laarMarker = Nothing
    }

-- | A list of aliases associated with the account.
laarAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarAccountAliases =
    lens _laarAccountAliases (\s a -> s { _laarAccountAliases = a })

-- | A flag that indicates whether there are more account aliases to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more account aliases in the
-- list.
laarIsTruncated :: Lens' ListAccountAliasesResponse Bool
laarIsTruncated = lens _laarIsTruncated (\s a -> s { _laarIsTruncated = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
laarMarker :: Lens' ListAccountAliasesResponse (Maybe Text)
laarMarker = lens _laarMarker (\s a -> s { _laarMarker = a })

instance FromXML ListAccountAliasesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListAccountAliases where
    type Sv ListAccountAliases = IAM
    type Rs ListAccountAliases = ListAccountAliasesResponse

    request = post "ListAccountAliases"
    response _ = xmlResponse

instance AWSPager ListAccountAliases where
    next rq rs
        | not (rs ^. laarIsTruncated) = Nothing
        | otherwise = Just $
            rq & laaMarker .~ rs ^. laarMarker
