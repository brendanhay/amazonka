{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to About Instance Profiles. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfilesForRole
-- &MaxItems=100 &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS false
-- AIPACZLS2EYYXMEXAMPLE /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYK3EXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z 6a8c3992-99f4-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
    (
    -- * Request
      ListInstanceProfilesForRole
    -- ** Request constructor
    , listInstanceProfilesForRole
    -- ** Request lenses
    , lipfrrRoleName
    , lipfrrMarker
    , lipfrrMaxItems

    -- * Response
    , ListInstanceProfilesForRoleResponse
    -- ** Response lenses
    , lipfrsIsTruncated
    , lipfrsInstanceProfiles
    , lipfrsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListInstanceProfilesForRole' request.
listInstanceProfilesForRole :: Text -- ^ 'lipfrrRoleName'
                            -> ListInstanceProfilesForRole
listInstanceProfilesForRole p1 = ListInstanceProfilesForRole
    { _lipfrrRoleName = p1
    , _lipfrrMarker = Nothing
    , _lipfrrMaxItems = Nothing
    }

data ListInstanceProfilesForRole = ListInstanceProfilesForRole
    { _lipfrrRoleName :: Text
      -- ^ The name of the role to list instance profiles for.
    , _lipfrrMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _lipfrrMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | The name of the role to list instance profiles for.
lipfrrRoleName
    :: Functor f
    => (Text
    -> f (Text))
    -> ListInstanceProfilesForRole
    -> f ListInstanceProfilesForRole
lipfrrRoleName f x =
    (\y -> x { _lipfrrRoleName = y })
       <$> f (_lipfrrRoleName x)
{-# INLINE lipfrrRoleName #-}

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lipfrrMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListInstanceProfilesForRole
    -> f ListInstanceProfilesForRole
lipfrrMarker f x =
    (\y -> x { _lipfrrMarker = y })
       <$> f (_lipfrrMarker x)
{-# INLINE lipfrrMarker #-}

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lipfrrMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListInstanceProfilesForRole
    -> f ListInstanceProfilesForRole
lipfrrMaxItems f x =
    (\y -> x { _lipfrrMaxItems = y })
       <$> f (_lipfrrMaxItems x)
{-# INLINE lipfrrMaxItems #-}

instance ToQuery ListInstanceProfilesForRole where
    toQuery = genericQuery def

data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { _lipfrsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more instance profiles to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more instance profiles in the list.
    , _lipfrsInstanceProfiles :: [InstanceProfile]
      -- ^ A list of instance profiles.
    , _lipfrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more instance profiles to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more instance profiles in
-- the list.
lipfrsIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListInstanceProfilesForRoleResponse
    -> f ListInstanceProfilesForRoleResponse
lipfrsIsTruncated f x =
    (\y -> x { _lipfrsIsTruncated = y })
       <$> f (_lipfrsIsTruncated x)
{-# INLINE lipfrsIsTruncated #-}

-- | A list of instance profiles.
lipfrsInstanceProfiles
    :: Functor f
    => ([InstanceProfile]
    -> f ([InstanceProfile]))
    -> ListInstanceProfilesForRoleResponse
    -> f ListInstanceProfilesForRoleResponse
lipfrsInstanceProfiles f x =
    (\y -> x { _lipfrsInstanceProfiles = y })
       <$> f (_lipfrsInstanceProfiles x)
{-# INLINE lipfrsInstanceProfiles #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lipfrsMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListInstanceProfilesForRoleResponse
    -> f ListInstanceProfilesForRoleResponse
lipfrsMarker f x =
    (\y -> x { _lipfrsMarker = y })
       <$> f (_lipfrsMarker x)
{-# INLINE lipfrsMarker #-}

instance FromXML ListInstanceProfilesForRoleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInstanceProfilesForRole where
    type Sv ListInstanceProfilesForRole = IAM
    type Rs ListInstanceProfilesForRole = ListInstanceProfilesForRoleResponse

    request = post "ListInstanceProfilesForRole"
    response _ = xmlResponse

instance AWSPager ListInstanceProfilesForRole where
    next rq rs
        | not (_lipfrsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lipfrrMarker = _lipfrsMarker rs
            }
