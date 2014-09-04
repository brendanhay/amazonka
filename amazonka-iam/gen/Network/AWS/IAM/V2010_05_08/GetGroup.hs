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
    , getGroup
    -- ** Request lenses
    , ggrGroupName
    , ggrMarker
    , ggrMaxItems

    -- * Response
    , GetGroupResponse
    -- ** Response lenses
    , ggsIsTruncated
    , ggsGroup
    , ggsUsers
    , ggsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetGroup' request.
getGroup :: Text -- ^ 'ggrGroupName'
         -> GetGroup
getGroup p1 = GetGroup
    { _ggrGroupName = p1
    , _ggrMarker = Nothing
    , _ggrMaxItems = Nothing
    }
{-# INLINE getGroup #-}

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
ggrGroupName f x =
    f (_ggrGroupName x)
        <&> \y -> x { _ggrGroupName = y }
{-# INLINE ggrGroupName #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
ggrMarker :: Lens' GetGroup (Maybe Text)
ggrMarker f x =
    f (_ggrMarker x)
        <&> \y -> x { _ggrMarker = y }
{-# INLINE ggrMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- user names you want in the response. If there are additional user names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
ggrMaxItems :: Lens' GetGroup (Maybe Integer)
ggrMaxItems f x =
    f (_ggrMaxItems x)
        <&> \y -> x { _ggrMaxItems = y }
{-# INLINE ggrMaxItems #-}

instance ToQuery GetGroup where
    toQuery = genericQuery def

data GetGroupResponse = GetGroupResponse
    { _ggsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more user names to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more user names in the list.
    , _ggsGroup :: Group
      -- ^ Information about the group.
    , _ggsUsers :: [User]
      -- ^ A list of users in the group.
    , _ggsMarker :: Maybe Text
      -- ^ If IsTruncated is true, then this element is present and contains
      -- the value to use for the Marker parameter in a subsequent
      -- pagination request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more user names in the list.
ggsIsTruncated :: Lens' GetGroupResponse (Bool)
ggsIsTruncated f x =
    f (_ggsIsTruncated x)
        <&> \y -> x { _ggsIsTruncated = y }
{-# INLINE ggsIsTruncated #-}

-- | Information about the group.
ggsGroup :: Lens' GetGroupResponse (Group)
ggsGroup f x =
    f (_ggsGroup x)
        <&> \y -> x { _ggsGroup = y }
{-# INLINE ggsGroup #-}

-- | A list of users in the group.
ggsUsers :: Lens' GetGroupResponse ([User])
ggsUsers f x =
    f (_ggsUsers x)
        <&> \y -> x { _ggsUsers = y }
{-# INLINE ggsUsers #-}

-- | If IsTruncated is true, then this element is present and contains the value
-- to use for the Marker parameter in a subsequent pagination request.
ggsMarker :: Lens' GetGroupResponse (Maybe Text)
ggsMarker f x =
    f (_ggsMarker x)
        <&> \y -> x { _ggsMarker = y }
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
