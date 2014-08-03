{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.IAM.V2010_05_08.GetGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetGroup' request.
getGroup :: Text -- ^ '_ggrGroupName'
         -> GetGroup
getGroup p1 = GetGroup
    { _ggrGroupName = p1
    , _ggrMarker = Nothing
    , _ggrMaxItems = Nothing
    }

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
    } deriving (Generic)

makeLenses ''GetGroup

instance ToQuery GetGroup where
    toQuery = genericToQuery def

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
    } deriving (Generic)

makeLenses ''GetGroupResponse

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
