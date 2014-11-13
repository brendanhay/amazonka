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

-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Reboots a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.RebootInstance
    (
    -- * Request
      RebootInstance
    -- ** Request constructor
    , rebootInstance
    -- ** Request lenses
    , riInstanceId

    -- * Response
    , RebootInstanceResponse
    -- ** Response constructor
    , rebootInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype RebootInstance = RebootInstance
    { _riInstanceId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'RebootInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riInstanceId' @::@ 'Text'
--
rebootInstance :: Text -- ^ 'riInstanceId'
               -> RebootInstance
rebootInstance p1 = RebootInstance
    { _riInstanceId = p1
    }

-- | The instance ID.
riInstanceId :: Lens' RebootInstance Text
riInstanceId = lens _riInstanceId (\s a -> s { _riInstanceId = a })

instance ToPath RebootInstance where
    toPath = const "/"

instance ToQuery RebootInstance where
    toQuery = const mempty

instance ToHeaders RebootInstance

instance ToBody RebootInstance where
    toBody = toBody . encode . _riInstanceId

data RebootInstanceResponse = RebootInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RebootInstanceResponse' constructor.
rebootInstanceResponse :: RebootInstanceResponse
rebootInstanceResponse = RebootInstanceResponse

-- FromJSON

instance AWSRequest RebootInstance where
    type Sv RebootInstance = OpsWorks
    type Rs RebootInstance = RebootInstanceResponse

    request  = post'
    response = nullaryResponse RebootInstanceResponse
