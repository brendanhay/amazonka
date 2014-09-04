{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListGroupsForUser
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
module Network.AWS.IAM.V2010_05_08.ListGroupsForUser
    (
    -- * Request
      ListGroupsForUser
    -- ** Request constructor
    , listGroupsForUser
    -- ** Request lenses
    , lgfurUserName
    , lgfurMarker
    , lgfurMaxItems

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response lenses
    , lgfusIsTruncated
    , lgfusGroups
    , lgfusMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListGroupsForUser' request.
listGroupsForUser :: Text -- ^ 'lgfurUserName'
                  -> ListGroupsForUser
listGroupsForUser p1 = ListGroupsForUser
    { _lgfurUserName = p1
    , _lgfurMarker = Nothing
    , _lgfurMaxItems = Nothing
    }
{-# INLINE listGroupsForUser #-}

data ListGroupsForUser = ListGroupsForUser
    { _lgfurUserName :: Text
      -- ^ The name of the user to list groups for.
    , _lgfurMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lgfurMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of groups you want in the response. If there are
      -- additional groups beyond the maximum you specify, the IsTruncated
      -- response element is true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | The name of the user to list groups for.
lgfurUserName :: Lens' ListGroupsForUser (Text)
lgfurUserName f x =
    f (_lgfurUserName x)
        <&> \y -> x { _lgfurUserName = y }
{-# INLINE lgfurUserName #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgfurMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfurMarker f x =
    f (_lgfurMarker x)
        <&> \y -> x { _lgfurMarker = y }
{-# INLINE lgfurMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond the
-- maximum you specify, the IsTruncated response element is true. This
-- parameter is optional. If you do not include it, it defaults to 100.
lgfurMaxItems :: Lens' ListGroupsForUser (Maybe Integer)
lgfurMaxItems f x =
    f (_lgfurMaxItems x)
        <&> \y -> x { _lgfurMaxItems = y }
{-# INLINE lgfurMaxItems #-}

instance ToQuery ListGroupsForUser where
    toQuery = genericQuery def

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { _lgfusIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more groups to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more
      -- groups in the list.
    , _lgfusGroups :: [Group]
      -- ^ A list of groups.
    , _lgfusMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more groups in the list.
lgfusIsTruncated :: Lens' ListGroupsForUserResponse (Bool)
lgfusIsTruncated f x =
    f (_lgfusIsTruncated x)
        <&> \y -> x { _lgfusIsTruncated = y }
{-# INLINE lgfusIsTruncated #-}

-- | A list of groups.
lgfusGroups :: Lens' ListGroupsForUserResponse ([Group])
lgfusGroups f x =
    f (_lgfusGroups x)
        <&> \y -> x { _lgfusGroups = y }
{-# INLINE lgfusGroups #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgfusMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfusMarker f x =
    f (_lgfusMarker x)
        <&> \y -> x { _lgfusMarker = y }
{-# INLINE lgfusMarker #-}

instance FromXML ListGroupsForUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroupsForUser where
    type Sv ListGroupsForUser = IAM
    type Rs ListGroupsForUser = ListGroupsForUserResponse

    request = post "ListGroupsForUser"
    response _ = xmlResponse

instance AWSPager ListGroupsForUser where
    next rq rs
        | not (_lgfusIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lgfurMarker = _lgfusMarker rs
            }
