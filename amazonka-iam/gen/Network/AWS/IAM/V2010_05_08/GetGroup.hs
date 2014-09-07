{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetGroup
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
module Network.AWS.IAM.V2010_05_08.GetGroup
    (
    -- * Request
      GetGroup
    -- ** Request constructor
    , mkGetGroup
    -- ** Request lenses
    , ggGroupName
    , ggMarker
    , ggMaxItems

    -- * Response
    , GetGroupResponse
    -- ** Response lenses
    , ggrsGroup
    , ggrsUsers
    , ggrsIsTruncated
    , ggrsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data GetGroup = GetGroup
    { _ggGroupName :: Text
    , _ggMarker :: Maybe Text
    , _ggMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroup' request.
mkGetGroup :: Text -- ^ 'ggGroupName'
           -> GetGroup
mkGetGroup p1 = GetGroup
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
    { _ggrsGroup :: Group
    , _ggrsUsers :: [User]
    , _ggrsIsTruncated :: Bool
    , _ggrsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Information about the group.
ggrsGroup :: Lens' GetGroupResponse Group
ggrsGroup = lens _ggrsGroup (\s a -> s { _ggrsGroup = a })

-- | A list of users in the group.
ggrsUsers :: Lens' GetGroupResponse [User]
ggrsUsers = lens _ggrsUsers (\s a -> s { _ggrsUsers = a })

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more user names in the list.
ggrsIsTruncated :: Lens' GetGroupResponse Bool
ggrsIsTruncated = lens _ggrsIsTruncated (\s a -> s { _ggrsIsTruncated = a })

-- | If IsTruncated is true, then this element is present and contains the value
-- to use for the Marker parameter in a subsequent pagination request.
ggrsMarker :: Lens' GetGroupResponse (Maybe Text)
ggrsMarker = lens _ggrsMarker (\s a -> s { _ggrsMarker = a })

instance FromXML GetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroup where
    type Sv GetGroup = IAM
    type Rs GetGroup = GetGroupResponse

    request = post "GetGroup"
    response _ = xmlResponse

instance AWSPager GetGroup where
    next rq rs
        | not (rs ^. ggrsIsTruncated) = Nothing
        | otherwise = Just $
            rq & ggMarker .~ rs ^. ggrsMarker
