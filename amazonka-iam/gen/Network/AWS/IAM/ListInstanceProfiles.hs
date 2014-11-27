{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the instance profiles that have the specified path prefix. If there
-- are none, the action returns an empty list. For more information about
-- instance profiles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- You can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfiles.html>
module Network.AWS.IAM.ListInstanceProfiles
    (
    -- * Request
      ListInstanceProfiles
    -- ** Request constructor
    , listInstanceProfiles
    -- ** Request lenses
    , lipMarker
    , lipMaxItems
    , lipPathPrefix

    -- * Response
    , ListInstanceProfilesResponse
    -- ** Response constructor
    , listInstanceProfilesResponse
    -- ** Response lenses
    , liprInstanceProfiles
    , liprIsTruncated
    , liprMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListInstanceProfiles = ListInstanceProfiles
    { _lipMarker     :: Maybe Text
    , _lipMaxItems   :: Maybe Nat
    , _lipPathPrefix :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListInstanceProfiles' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipMarker' @::@ 'Maybe' 'Text'
--
-- * 'lipMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lipPathPrefix' @::@ 'Maybe' 'Text'
--
listInstanceProfiles :: ListInstanceProfiles
listInstanceProfiles = ListInstanceProfiles
    { _lipPathPrefix = Nothing
    , _lipMarker     = Nothing
    , _lipMaxItems   = Nothing
    }

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated. Set
-- it to the value of the 'Marker' element in the response you just received.
lipMarker :: Lens' ListInstanceProfiles (Maybe Text)
lipMarker = lens _lipMarker (\s a -> s { _lipMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of instance profiles you want in the response. If there are additional
-- instance profiles beyond the maximum you specify, the 'IsTruncated' response
-- element is 'true'. This parameter is optional. If you do not include it, it
-- defaults to 100.
lipMaxItems :: Lens' ListInstanceProfiles (Maybe Natural)
lipMaxItems = lens _lipMaxItems (\s a -> s { _lipMaxItems = a }) . mapping _Nat

-- | The path prefix for filtering the results. For example, the prefix '/application_abc/component_xyz/' gets all instance profiles whose path starts with '/application_abc/component_xyz/'.
--
-- This parameter is optional. If it is not included, it defaults to a slash
-- (/), listing all instance profiles.
lipPathPrefix :: Lens' ListInstanceProfiles (Maybe Text)
lipPathPrefix = lens _lipPathPrefix (\s a -> s { _lipPathPrefix = a })

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { _liprInstanceProfiles :: List "member" InstanceProfile
    , _liprIsTruncated      :: Maybe Bool
    , _liprMarker           :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListInstanceProfilesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liprInstanceProfiles' @::@ ['InstanceProfile']
--
-- * 'liprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'liprMarker' @::@ 'Maybe' 'Text'
--
listInstanceProfilesResponse :: ListInstanceProfilesResponse
listInstanceProfilesResponse = ListInstanceProfilesResponse
    { _liprInstanceProfiles = mempty
    , _liprIsTruncated      = Nothing
    , _liprMarker           = Nothing
    }

-- | A list of instance profiles.
liprInstanceProfiles :: Lens' ListInstanceProfilesResponse [InstanceProfile]
liprInstanceProfiles =
    lens _liprInstanceProfiles (\s a -> s { _liprInstanceProfiles = a })
        . _List

-- | A flag that indicates whether there are more instance profiles to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more instance profiles in the
-- list.
liprIsTruncated :: Lens' ListInstanceProfilesResponse (Maybe Bool)
liprIsTruncated = lens _liprIsTruncated (\s a -> s { _liprIsTruncated = a })

-- | If 'IsTruncated' is 'true', this element is present and contains the value to
-- use for the 'Marker' parameter in a subsequent pagination request.
liprMarker :: Lens' ListInstanceProfilesResponse (Maybe Text)
liprMarker = lens _liprMarker (\s a -> s { _liprMarker = a })

instance ToPath ListInstanceProfiles where
    toPath = const "/"

instance ToQuery ListInstanceProfiles where
    toQuery ListInstanceProfiles{..} = mconcat
        [ "Marker"     =? _lipMarker
        , "MaxItems"   =? _lipMaxItems
        , "PathPrefix" =? _lipPathPrefix
        ]

instance ToHeaders ListInstanceProfiles

instance AWSRequest ListInstanceProfiles where
    type Sv ListInstanceProfiles = IAM
    type Rs ListInstanceProfiles = ListInstanceProfilesResponse

    request  = post "ListInstanceProfiles"
    response = xmlResponse

instance FromXML ListInstanceProfilesResponse where
    parseXML = withElement "ListInstanceProfilesResult" $ \x -> ListInstanceProfilesResponse
        <$> x .@  "InstanceProfiles"
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"

instance AWSPager ListInstanceProfiles where
    page rq rs
        | stop (rs ^. liprIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lipMarker .~ rs ^. liprMarker
