{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to About Instance Profiles. For more information
-- about ARNs, go to ARNs.
module Network.AWS.IAM.GetInstanceProfile
    (
    -- * Request
      GetInstanceProfile
    -- ** Request constructor
    , getInstanceProfile
    -- ** Request lenses
    , gipInstanceProfileName

    -- * Response
    , GetInstanceProfileResponse
    -- ** Response constructor
    , getInstanceProfileResponse
    -- ** Response lenses
    , giprInstanceProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype GetInstanceProfile = GetInstanceProfile
    { _gipInstanceProfileName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipInstanceProfileName' @::@ 'Text'
--
getInstanceProfile :: Text -- ^ 'gipInstanceProfileName'
                   -> GetInstanceProfile
getInstanceProfile p1 = GetInstanceProfile
    { _gipInstanceProfileName = p1
    }

-- | The name of the instance profile to get information about.
gipInstanceProfileName :: Lens' GetInstanceProfile Text
gipInstanceProfileName =
    lens _gipInstanceProfileName (\s a -> s { _gipInstanceProfileName = a })

instance ToPath GetInstanceProfile where
    toPath = const "/"

instance ToQuery GetInstanceProfile

newtype GetInstanceProfileResponse = GetInstanceProfileResponse
    { _giprInstanceProfile :: InstanceProfile
    } deriving (Eq, Show, Generic)

-- | 'GetInstanceProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprInstanceProfile' @::@ 'InstanceProfile'
--
getInstanceProfileResponse :: InstanceProfile -- ^ 'giprInstanceProfile'
                           -> GetInstanceProfileResponse
getInstanceProfileResponse p1 = GetInstanceProfileResponse
    { _giprInstanceProfile = p1
    }

-- | Information about the instance profile.
giprInstanceProfile :: Lens' GetInstanceProfileResponse InstanceProfile
giprInstanceProfile =
    lens _giprInstanceProfile (\s a -> s { _giprInstanceProfile = a })

instance AWSRequest GetInstanceProfile where
    type Sv GetInstanceProfile = IAM
    type Rs GetInstanceProfile = GetInstanceProfileResponse

    request  = post "GetInstanceProfile"
    response = xmlResponse $ \h x -> GetInstanceProfileResponse
        <$> x %| "InstanceProfile"
