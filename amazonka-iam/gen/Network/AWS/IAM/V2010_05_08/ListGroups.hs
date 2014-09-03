{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroups
-- &PathPrefix=/division_abc/subdivision_xyz/ &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/ Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test
-- AGP2MAB8DPLSRHEXAMPLE arn:aws:iam::123456789012:group
-- /division_abc/subdivision_xyz/product_1234/engineering/Test
-- /division_abc/subdivision_xyz/product_1234/ Managers AGPIODR4TAW7CSEXAMPLE
-- arn:aws:iam::123456789012
-- :group/division_abc/subdivision_xyz/product_1234/Managers false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListGroups
    (
    -- * Request
      ListGroups
    -- ** Request constructor
    , listGroups
    -- ** Request lenses
    , lgrMarker
    , lgrMaxItems
    , lgrPathPrefix

    -- * Response
    , ListGroupsResponse
    -- ** Response lenses
    , lgsIsTruncated
    , lgsGroups
    , lgsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListGroups' request.
listGroups :: ListGroups
listGroups = ListGroups
    { _lgrMarker = Nothing
    , _lgrMaxItems = Nothing
    , _lgrPathPrefix = Nothing
    }

data ListGroups = ListGroups
    { _lgrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lgrMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of groups you want in the response. If there are
      -- additional groups beyond the maximum you specify, the IsTruncated
      -- response element is true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    , _lgrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all groups whose
      -- path starts with /division_abc/subdivision_xyz/. This parameter
      -- is optional. If it is not included, it defaults to a slash (/),
      -- listing all groups.
    } deriving (Show, Generic)

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgrMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGroups
    -> f ListGroups
lgrMarker f x =
    (\y -> x { _lgrMarker = y })
       <$> f (_lgrMarker x)
{-# INLINE lgrMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond the
-- maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgrMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListGroups
    -> f ListGroups
lgrMaxItems f x =
    (\y -> x { _lgrMaxItems = y })
       <$> f (_lgrMaxItems x)
{-# INLINE lgrMaxItems #-}

-- | The path prefix for filtering the results. For example:
-- /division_abc/subdivision_xyz/, which would get all groups whose path
-- starts with /division_abc/subdivision_xyz/. This parameter is optional. If
-- it is not included, it defaults to a slash (/), listing all groups.
lgrPathPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGroups
    -> f ListGroups
lgrPathPrefix f x =
    (\y -> x { _lgrPathPrefix = y })
       <$> f (_lgrPathPrefix x)
{-# INLINE lgrPathPrefix #-}

instance ToQuery ListGroups where
    toQuery = genericQuery def

data ListGroupsResponse = ListGroupsResponse
    { _lgsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more groups to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more
      -- groups in the list.
    , _lgsGroups :: [Group]
      -- ^ A list of groups.
    , _lgsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more groups in the list.
lgsIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListGroupsResponse
    -> f ListGroupsResponse
lgsIsTruncated f x =
    (\y -> x { _lgsIsTruncated = y })
       <$> f (_lgsIsTruncated x)
{-# INLINE lgsIsTruncated #-}

-- | A list of groups.
lgsGroups
    :: Functor f
    => ([Group]
    -> f ([Group]))
    -> ListGroupsResponse
    -> f ListGroupsResponse
lgsGroups f x =
    (\y -> x { _lgsGroups = y })
       <$> f (_lgsGroups x)
{-# INLINE lgsGroups #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgsMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGroupsResponse
    -> f ListGroupsResponse
lgsMarker f x =
    (\y -> x { _lgsMarker = y })
       <$> f (_lgsMarker x)
{-# INLINE lgsMarker #-}

instance FromXML ListGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroups where
    type Sv ListGroups = IAM
    type Rs ListGroups = ListGroupsResponse

    request = post "ListGroups"
    response _ = xmlResponse

instance AWSPager ListGroups where
    next rq rs
        | not (_lgsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lgrMarker = _lgsMarker rs
            }
