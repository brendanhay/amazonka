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
    , mkGetGroupRequest
    -- ** Request lenses
    , ggrGroupName
    , ggrMarker
    , ggrMaxItems

    -- * Response
    , GetGroupResponse
    -- ** Response lenses
    , ggsGroup
    , ggsUsers
    , ggsIsTruncated
    , ggsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroup' request.
mkGetGroupRequest :: Text -- ^ 'ggrGroupName'
                  -> GetGroup
mkGetGroupRequest p1 = GetGroup
    { _ggrGroupName = p1
    , _ggrMarker = Nothing
    , _ggrMaxItems = Nothing
    }
{-# INLINE mkGetGroupRequest #-}

data GetGroup = GetGroup
    { _ggrGroupName :: Text
      -- ^ Name of the group.
    , _ggrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _ggrMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of user names you want in the response. If there are
      -- additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | Name of the group.
ggrGroupName :: Lens' GetGroup (Text)
ggrGroupName = lens _ggrGroupName (\s a -> s { _ggrGroupName = a })
{-# INLINE ggrGroupName #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
ggrMarker :: Lens' GetGroup (Maybe Text)
ggrMarker = lens _ggrMarker (\s a -> s { _ggrMarker = a })
{-# INLINE ggrMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- user names you want in the response. If there are additional user names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
ggrMaxItems :: Lens' GetGroup (Maybe Integer)
ggrMaxItems = lens _ggrMaxItems (\s a -> s { _ggrMaxItems = a })
{-# INLINE ggrMaxItems #-}

instance ToQuery GetGroup where
    toQuery = genericQuery def

data GetGroupResponse = GetGroupResponse
    { _ggsGroup :: Group
      -- ^ Information about the group.
    , _ggsUsers :: [User]
      -- ^ A list of users in the group.
    , _ggsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more user names to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more user names in the list.
    , _ggsMarker :: Maybe Text
      -- ^ If IsTruncated is true, then this element is present and contains
      -- the value to use for the Marker parameter in a subsequent
      -- pagination request.
    } deriving (Show, Generic)

-- | Information about the group.
ggsGroup :: Lens' GetGroupResponse (Group)
ggsGroup = lens _ggsGroup (\s a -> s { _ggsGroup = a })
{-# INLINE ggsGroup #-}

-- | A list of users in the group.
ggsUsers :: Lens' GetGroupResponse ([User])
ggsUsers = lens _ggsUsers (\s a -> s { _ggsUsers = a })
{-# INLINE ggsUsers #-}

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more user names in the list.
ggsIsTruncated :: Lens' GetGroupResponse (Bool)
ggsIsTruncated = lens _ggsIsTruncated (\s a -> s { _ggsIsTruncated = a })
{-# INLINE ggsIsTruncated #-}

-- | If IsTruncated is true, then this element is present and contains the value
-- to use for the Marker parameter in a subsequent pagination request.
ggsMarker :: Lens' GetGroupResponse (Maybe Text)
ggsMarker = lens _ggsMarker (\s a -> s { _ggsMarker = a })
{-# INLINE ggsMarker #-}

instance FromXML GetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroup where
    type Sv GetGroup = IAM
    type Rs GetGroup = GetGroupResponse

    request = post "GetGroup"
    response _ = xmlResponse

instance AWSPager GetGroup where
    next rq rs
        | not (_ggsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _ggrMarker = _ggsMarker rs
            }
