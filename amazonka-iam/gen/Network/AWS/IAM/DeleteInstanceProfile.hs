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

-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified instance profile. The instance profile must not have
-- an associated role.
--
-- Make sure you do not have any Amazon EC2 instances running with the
-- instance profile you are about to delete. Deleting a role or instance profile
-- that is associated with a running instance will break any applications
-- running on the instance.   For more information about instance profiles, go
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteInstanceProfile.html>
module Network.AWS.IAM.DeleteInstanceProfile
    (
    -- * Request
      DeleteInstanceProfile
    -- ** Request constructor
    , deleteInstanceProfile
    -- ** Request lenses
    , dipInstanceProfileName

    -- * Response
    , DeleteInstanceProfileResponse
    -- ** Response constructor
    , deleteInstanceProfileResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype DeleteInstanceProfile = DeleteInstanceProfile
    { _dipInstanceProfileName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipInstanceProfileName' @::@ 'Text'
--
deleteInstanceProfile :: Text -- ^ 'dipInstanceProfileName'
                      -> DeleteInstanceProfile
deleteInstanceProfile p1 = DeleteInstanceProfile
    { _dipInstanceProfileName = p1
    }

-- | The name of the instance profile to delete.
dipInstanceProfileName :: Lens' DeleteInstanceProfile Text
dipInstanceProfileName =
    lens _dipInstanceProfileName (\s a -> s { _dipInstanceProfileName = a })

data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteInstanceProfileResponse' constructor.
deleteInstanceProfileResponse :: DeleteInstanceProfileResponse
deleteInstanceProfileResponse = DeleteInstanceProfileResponse

instance ToPath DeleteInstanceProfile where
    toPath = const "/"

instance ToQuery DeleteInstanceProfile where
    toQuery DeleteInstanceProfile{..} = mconcat
        [ "InstanceProfileName" =? _dipInstanceProfileName
        ]

instance ToHeaders DeleteInstanceProfile

instance AWSRequest DeleteInstanceProfile where
    type Sv DeleteInstanceProfile = IAM
    type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse

    request  = post "DeleteInstanceProfile"
    response = nullResponse DeleteInstanceProfileResponse
