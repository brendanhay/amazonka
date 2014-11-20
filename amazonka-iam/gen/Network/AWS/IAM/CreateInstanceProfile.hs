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

-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new instance profile. For information about instance profiles, go
-- to About Instance Profiles. For information about the number of instance
-- profiles you can create, see Limitations on IAM Entities in the Using IAM
-- guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfile.html>
module Network.AWS.IAM.CreateInstanceProfile
    (
    -- * Request
      CreateInstanceProfile
    -- ** Request constructor
    , createInstanceProfile
    -- ** Request lenses
    , cipInstanceProfileName
    , cipPath

    -- * Response
    , CreateInstanceProfileResponse
    -- ** Response constructor
    , createInstanceProfileResponse
    -- ** Response lenses
    , ciprInstanceProfile
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateInstanceProfile = CreateInstanceProfile
    { _cipInstanceProfileName :: Text
    , _cipPath                :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cipInstanceProfileName' @::@ 'Text'
--
-- * 'cipPath' @::@ 'Maybe' 'Text'
--
createInstanceProfile :: Text -- ^ 'cipInstanceProfileName'
                      -> CreateInstanceProfile
createInstanceProfile p1 = CreateInstanceProfile
    { _cipInstanceProfileName = p1
    , _cipPath                = Nothing
    }

-- | The name of the instance profile to create.
cipInstanceProfileName :: Lens' CreateInstanceProfile Text
cipInstanceProfileName =
    lens _cipInstanceProfileName (\s a -> s { _cipInstanceProfileName = a })

-- | The path to the instance profile. For more information about paths, see
-- IAM Identifiers in the Using IAM guide. This parameter is optional. If it
-- is not included, it defaults to a slash (/).
cipPath :: Lens' CreateInstanceProfile (Maybe Text)
cipPath = lens _cipPath (\s a -> s { _cipPath = a })

newtype CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { _ciprInstanceProfile :: InstanceProfile
    } deriving (Eq, Show)

-- | 'CreateInstanceProfileResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciprInstanceProfile' @::@ 'InstanceProfile'
--
createInstanceProfileResponse :: InstanceProfile -- ^ 'ciprInstanceProfile'
                              -> CreateInstanceProfileResponse
createInstanceProfileResponse p1 = CreateInstanceProfileResponse
    { _ciprInstanceProfile = p1
    }

-- | Information about the instance profile.
ciprInstanceProfile :: Lens' CreateInstanceProfileResponse InstanceProfile
ciprInstanceProfile =
    lens _ciprInstanceProfile (\s a -> s { _ciprInstanceProfile = a })

instance ToPath CreateInstanceProfile where
    toPath = const "/"

instance ToQuery CreateInstanceProfile where
    toQuery CreateInstanceProfile{..} = mconcat
        [ "InstanceProfileName" =? _cipInstanceProfileName
        , "Path"                =? _cipPath
        ]

instance ToHeaders CreateInstanceProfile

instance AWSRequest CreateInstanceProfile where
    type Sv CreateInstanceProfile = IAM
    type Rs CreateInstanceProfile = CreateInstanceProfileResponse

    request  = post "CreateInstanceProfile"
    response = xmlResponse

instance FromXML CreateInstanceProfileResponse where
    parseXML = withElement "CreateInstanceProfileResult" $ \x -> CreateInstanceProfileResponse
        <$> x .@  "InstanceProfile"
