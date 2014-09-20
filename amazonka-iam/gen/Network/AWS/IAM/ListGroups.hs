{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroups
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
module Network.AWS.IAM.ListGroups
    (
    -- * Request
      ListGroups
    -- ** Request constructor
    , listGroups
    -- ** Request lenses
    , lgPathPrefix
    , lgMarker
    , lgMaxItems

    -- * Response
    , ListGroupsResponse
    -- ** Response constructor
    , listGroupsResponse
    -- ** Response lenses
    , lgrGroups
    , lgrIsTruncated
    , lgrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListGroups = ListGroups
    { _lgPathPrefix :: Maybe Text
    , _lgMarker :: Maybe Text
    , _lgMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PathPrefix ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listGroups :: ListGroups
listGroups = ListGroups
    { _lgPathPrefix = Nothing
    , _lgMarker = Nothing
    , _lgMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- /division_abc/subdivision_xyz/, which would get all groups whose path
-- starts with /division_abc/subdivision_xyz/. This parameter is optional. If
-- it is not included, it defaults to a slash (/), listing all groups.
lgPathPrefix :: Lens' ListGroups (Maybe Text)
lgPathPrefix = lens _lgPathPrefix (\s a -> s { _lgPathPrefix = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgMarker :: Lens' ListGroups (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond the
-- maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgMaxItems :: Lens' ListGroups (Maybe Integer)
lgMaxItems = lens _lgMaxItems (\s a -> s { _lgMaxItems = a })

instance ToQuery ListGroups where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListGroups action.
data ListGroupsResponse = ListGroupsResponse
    { _lgrGroups :: [Group]
    , _lgrIsTruncated :: !Bool
    , _lgrMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Groups ::@ @[Group]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
listGroupsResponse :: [Group] -- ^ 'lgrGroups'
                   -> Bool -- ^ 'lgrIsTruncated'
                   -> ListGroupsResponse
listGroupsResponse p1 p2 = ListGroupsResponse
    { _lgrGroups = p1
    , _lgrIsTruncated = p2
    , _lgrMarker = Nothing
    }

-- | A list of groups.
lgrGroups :: Lens' ListGroupsResponse [Group]
lgrGroups = lens _lgrGroups (\s a -> s { _lgrGroups = a })

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more groups in the list.
lgrIsTruncated :: Lens' ListGroupsResponse Bool
lgrIsTruncated = lens _lgrIsTruncated (\s a -> s { _lgrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgrMarker :: Lens' ListGroupsResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\s a -> s { _lgrMarker = a })

instance FromXML ListGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroups where
    type Sv ListGroups = IAM
    type Rs ListGroups = ListGroupsResponse

    request = post "ListGroups"
    response _ = xmlResponse

instance AWSPager ListGroups where
    next rq rs
        | not (rs ^. lgrIsTruncated) = Nothing
        | otherwise = Just $
            rq & lgMarker .~ rs ^. lgrMarker
