{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListUsers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the users that have the specified path prefix. If there are none, the
-- action returns an empty list. You can paginate the results using the
-- MaxItems and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListUsers
-- &PathPrefix=/division_abc/subdivision_xyz/product_1234/engineering/
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/engineering/
-- Andrew AID2MAB8DPLSRHEXAMPLE arn:aws:iam::123456789012:user
-- /division_abc/subdivision_xyz/engineering/Andrew
-- /division_abc/subdivision_xyz/engineering/ Jackie AIDIODR4TAW7CSEXAMPLE
-- arn:aws:iam::123456789012:user
-- /division_abc/subdivision_xyz/engineering/Jackie false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListUsers where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListUsers' request.
listUsers :: ListUsers
listUsers = ListUsers
    { _lurMarker = Nothing
    , _lurMaxItems = Nothing
    , _lurPathPrefix = Nothing
    }

data ListUsers = ListUsers
    { _lurMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _lurMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , _lurPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all user names
      -- whose path starts with /division_abc/subdivision_xyz/. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/), listing all user names.
    } deriving (Show, Generic)

makeLenses ''ListUsers

instance ToQuery ListUsers where
    toQuery = genericToQuery def

data ListUsersResponse = ListUsersResponse
    { _lusIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more user names to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more users in the list.
    , _lusUsers :: [User]
      -- ^ A list of users.
    , _lusMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

makeLenses ''ListUsersResponse

instance AWSRequest ListUsers where
    type Sv ListUsers = IAM
    type Rs ListUsers = ListUsersResponse

    request = post "ListUsers"
    response _ = cursorResponse $ \hs xml ->
        pure ListUsersResponse
            <*> xml %| "BooleanType"
            <*> xml %| "UserListType"
            <*> xml %|? "MarkerType"

instance AWSPager ListUsers where
    next rq rs
        | not (_lusIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lurMarker = _lusMarker rs
            }
