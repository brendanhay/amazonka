{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupsForUser &UserName=Bob
-- &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Request
      ListGroupsForUser
    -- ** Request constructor
    , listGroupsForUser
    -- ** Request lenses
    , lgfuUserName
    , lgfuMarker
    , lgfuMaxItems

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response constructor
    , listGroupsForUserResponse
    -- ** Response lenses
    , lgfurGroups
    , lgfurIsTruncated
    , lgfurMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListGroupsForUser = ListGroupsForUser
    { _lgfuUserName :: Text
    , _lgfuMarker :: Maybe Text
    , _lgfuMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupsForUser' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listGroupsForUser :: Text -- ^ 'lgfuUserName'
                  -> ListGroupsForUser
listGroupsForUser p1 = ListGroupsForUser
    { _lgfuUserName = p1
    , _lgfuMarker = Nothing
    , _lgfuMaxItems = Nothing
    }

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\s a -> s { _lgfuUserName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\s a -> s { _lgfuMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond the
-- maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Integer)
lgfuMaxItems = lens _lgfuMaxItems (\s a -> s { _lgfuMaxItems = a })

instance ToQuery ListGroupsForUser where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListGroupsForUser
-- action.
data ListGroupsForUserResponse = ListGroupsForUserResponse
    { _lgfurGroups :: [Group]
    , _lgfurIsTruncated :: !Bool
    , _lgfurMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupsForUserResponse' response.
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
listGroupsForUserResponse :: [Group] -- ^ 'lgfurGroups'
                          -> Bool -- ^ 'lgfurIsTruncated'
                          -> ListGroupsForUserResponse
listGroupsForUserResponse p1 p2 = ListGroupsForUserResponse
    { _lgfurGroups = p1
    , _lgfurIsTruncated = p2
    , _lgfurMarker = Nothing
    }

-- | A list of groups.
lgfurGroups :: Lens' ListGroupsForUserResponse [Group]
lgfurGroups = lens _lgfurGroups (\s a -> s { _lgfurGroups = a })

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more groups in the list.
lgfurIsTruncated :: Lens' ListGroupsForUserResponse Bool
lgfurIsTruncated =
    lens _lgfurIsTruncated (\s a -> s { _lgfurIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgfurMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfurMarker = lens _lgfurMarker (\s a -> s { _lgfurMarker = a })

instance FromXML ListGroupsForUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroupsForUser where
    type Sv ListGroupsForUser = IAM
    type Rs ListGroupsForUser = ListGroupsForUserResponse

    request = post "ListGroupsForUser"
    response _ = xmlResponse

instance AWSPager ListGroupsForUser where
    next rq rs
        | not (rs ^. lgfurIsTruncated) = Nothing
        | otherwise = Just $
            rq & lgfuMarker .~ rs ^. lgfurMarker
