{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListAccountAliases
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
module Network.AWS.IAM.V2010_05_08.ListAccountAliases
    (
    -- * Request
      ListAccountAliases
    -- ** Request constructor
    , listAccountAliases
    -- ** Request lenses
    , laarMarker
    , laarMaxItems

    -- * Response
    , ListAccountAliasesResponse
    -- ** Response lenses
    , laasAccountAliases
    , laasIsTruncated
    , laasMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListAccountAliases' request.
listAccountAliases :: ListAccountAliases
listAccountAliases = ListAccountAliases
    { _laarMarker = Nothing
    , _laarMaxItems = Nothing
    }

data ListAccountAliases = ListAccountAliases
    { _laarMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _laarMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of account aliases you want in the response. If there are
      -- additional account aliases beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
laarMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListAccountAliases
    -> f ListAccountAliases
laarMarker f x =
    (\y -> x { _laarMarker = y })
       <$> f (_laarMarker x)
{-# INLINE laarMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- account aliases you want in the response. If there are additional account
-- aliases beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
laarMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListAccountAliases
    -> f ListAccountAliases
laarMaxItems f x =
    (\y -> x { _laarMaxItems = y })
       <$> f (_laarMaxItems x)
{-# INLINE laarMaxItems #-}

instance ToQuery ListAccountAliases where
    toQuery = genericQuery def

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { _laasAccountAliases :: [Text]
      -- ^ A list of aliases associated with the account.
    , _laasIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more account aliases to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more account aliases in the list.
    , _laasMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    } deriving (Show, Generic)

-- | A list of aliases associated with the account.
laasAccountAliases
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ListAccountAliasesResponse
    -> f ListAccountAliasesResponse
laasAccountAliases f x =
    (\y -> x { _laasAccountAliases = y })
       <$> f (_laasAccountAliases x)
{-# INLINE laasAccountAliases #-}

-- | A flag that indicates whether there are more account aliases to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more account aliases in the
-- list.
laasIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListAccountAliasesResponse
    -> f ListAccountAliasesResponse
laasIsTruncated f x =
    (\y -> x { _laasIsTruncated = y })
       <$> f (_laasIsTruncated x)
{-# INLINE laasIsTruncated #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
laasMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListAccountAliasesResponse
    -> f ListAccountAliasesResponse
laasMarker f x =
    (\y -> x { _laasMarker = y })
       <$> f (_laasMarker x)
{-# INLINE laasMarker #-}

instance FromXML ListAccountAliasesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListAccountAliases where
    type Sv ListAccountAliases = IAM
    type Rs ListAccountAliases = ListAccountAliasesResponse

    request = post "ListAccountAliases"
    response _ = xmlResponse

instance AWSPager ListAccountAliases where
    next rq rs
        | not (_laasIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _laarMarker = _laasMarker rs
            }
