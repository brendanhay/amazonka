{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=GetGroup &GroupName=Admins
-- &Version=2010-05-08 &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- /division_abc/subdivision_xyz/ Susan AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Susan false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetGroup
    (
    -- * Request
      GetGroup
    -- ** Request constructor
    , getGroup
    -- ** Request lenses
    , ggGroupName
    , ggMarker
    , ggMaxItems

    -- * Response
    , GetGroupResponse
    -- ** Response constructor
    , getGroupResponse
    -- ** Response lenses
    , ggrGroup
    , ggrUsers
    , ggrIsTruncated
    , ggrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetGroup = GetGroup
    { _ggGroupName :: Text
    , _ggMarker :: Maybe Text
    , _ggMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
getGroup :: Text -- ^ 'ggGroupName'
         -> GetGroup
getGroup p1 = GetGroup
    { _ggGroupName = p1
    , _ggMarker = Nothing
    , _ggMaxItems = Nothing
    }

-- | Name of the group.
ggGroupName :: Lens' GetGroup Text
ggGroupName = lens _ggGroupName (\s a -> s { _ggGroupName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
ggMarker :: Lens' GetGroup (Maybe Text)
ggMarker = lens _ggMarker (\s a -> s { _ggMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- user names you want in the response. If there are additional user names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
ggMaxItems :: Lens' GetGroup (Maybe Integer)
ggMaxItems = lens _ggMaxItems (\s a -> s { _ggMaxItems = a })

instance ToQuery GetGroup where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetGroup action.
data GetGroupResponse = GetGroupResponse
    { _ggrGroup :: Group
    , _ggrUsers :: [User]
    , _ggrIsTruncated :: !Bool
    , _ggrMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Group ::@ @Group@
--
-- * @Users ::@ @[User]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
getGroupResponse :: Group -- ^ 'ggrGroup'
                 -> [User] -- ^ 'ggrUsers'
                 -> Bool -- ^ 'ggrIsTruncated'
                 -> GetGroupResponse
getGroupResponse p1 p2 p3 = GetGroupResponse
    { _ggrGroup = p1
    , _ggrUsers = p2
    , _ggrIsTruncated = p3
    , _ggrMarker = Nothing
    }

-- | Information about the group.
ggrGroup :: Lens' GetGroupResponse Group
ggrGroup = lens _ggrGroup (\s a -> s { _ggrGroup = a })

-- | A list of users in the group.
ggrUsers :: Lens' GetGroupResponse [User]
ggrUsers = lens _ggrUsers (\s a -> s { _ggrUsers = a })

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more user names in the list.
ggrIsTruncated :: Lens' GetGroupResponse Bool
ggrIsTruncated = lens _ggrIsTruncated (\s a -> s { _ggrIsTruncated = a })

-- | If IsTruncated is true, then this element is present and contains the value
-- to use for the Marker parameter in a subsequent pagination request.
ggrMarker :: Lens' GetGroupResponse (Maybe Text)
ggrMarker = lens _ggrMarker (\s a -> s { _ggrMarker = a })

instance FromXML GetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroup where
    type Sv GetGroup = IAM
    type Rs GetGroup = GetGroupResponse

    request = post "GetGroup"
    response _ = xmlResponse

instance AWSPager GetGroup where
    next rq rs
        | not (rs ^. ggrIsTruncated) = Nothing
        | otherwise = Just $
            rq & ggMarker .~ rs ^. ggrMarker
