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

-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters the specified AMI. After you deregister an AMI, it can't be
-- used to launch new instances. This command does not delete the AMI.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html>
module Network.AWS.EC2.DeregisterImage
    (
    -- * Request
      DeregisterImage
    -- ** Request constructor
    , deregisterImage
    -- ** Request lenses
    , diDryRun
    , diImageId

    -- * Response
    , DeregisterImageResponse
    -- ** Response constructor
    , deregisterImageResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeregisterImage = DeregisterImage
    { _diDryRun  :: Maybe Bool
    , _diImageId :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeregisterImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'diImageId' @::@ 'Text'
--
deregisterImage :: Text -- ^ 'diImageId'
                -> DeregisterImage
deregisterImage p1 = DeregisterImage
    { _diImageId = p1
    , _diDryRun  = Nothing
    }

diDryRun :: Lens' DeregisterImage (Maybe Bool)
diDryRun = lens _diDryRun (\s a -> s { _diDryRun = a })

-- | The ID of the AMI.
diImageId :: Lens' DeregisterImage Text
diImageId = lens _diImageId (\s a -> s { _diImageId = a })

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterImageResponse' constructor.
deregisterImageResponse :: DeregisterImageResponse
deregisterImageResponse = DeregisterImageResponse

instance ToPath DeregisterImage where
    toPath = const "/"

instance ToQuery DeregisterImage where
    toQuery DeregisterImage{..} = mconcat
        [ "dryRun"  =? _diDryRun
        , "ImageId" =? _diImageId
        ]

instance ToHeaders DeregisterImage

instance AWSRequest DeregisterImage where
    type Sv DeregisterImage = EC2
    type Rs DeregisterImage = DeregisterImageResponse

    request  = post "DeregisterImage"
    response = nullResponse DeregisterImageResponse


Some kind of operator / class to check the types whether to continue?
