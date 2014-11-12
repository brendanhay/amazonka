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
module Network.AWS.EC2.DeregisterImage
    (
    -- * Request
      DeregisterImage
    -- ** Request constructor
    , deregisterImage
    -- ** Request lenses
    , di2DryRun
    , di2ImageId

    -- * Response
    , DeregisterImageResponse
    -- ** Response constructor
    , deregisterImageResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeregisterImage = DeregisterImage
    { _di2DryRun  :: Maybe Bool
    , _di2ImageId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'di2ImageId' @::@ 'Text'
--
deregisterImage :: Text -- ^ 'di2ImageId'
                -> DeregisterImage
deregisterImage p1 = DeregisterImage
    { _di2ImageId = p1
    , _di2DryRun  = Nothing
    }

di2DryRun :: Lens' DeregisterImage (Maybe Bool)
di2DryRun = lens _di2DryRun (\s a -> s { _di2DryRun = a })

-- | The ID of the AMI.
di2ImageId :: Lens' DeregisterImage Text
di2ImageId = lens _di2ImageId (\s a -> s { _di2ImageId = a })

instance ToQuery DeregisterImage

instance ToPath DeregisterImage where
    toPath = const "/"

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterImageResponse' constructor.
deregisterImageResponse :: DeregisterImageResponse
deregisterImageResponse = DeregisterImageResponse

instance FromXML DeregisterImageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeregisterImageResponse"

instance AWSRequest DeregisterImage where
    type Sv DeregisterImage = EC2
    type Rs DeregisterImage = DeregisterImageResponse

    request  = post "DeregisterImage"
    response = nullaryResponse DeregisterImageResponse
