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

-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy created by PutScalingPolicy.
module Network.AWS.AutoScaling.DeletePolicy
    (
    -- * Request
      DeletePolicy
    -- ** Request constructor
    , deletePolicy
    -- ** Request lenses
    , dpAutoScalingGroupName
    , dpPolicyName

    -- * Response
    , DeletePolicyResponse
    -- ** Response constructor
    , deletePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeletePolicy = DeletePolicy
    { _dpAutoScalingGroupName :: Maybe Text
    , _dpPolicyName           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeletePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dpPolicyName' @::@ 'Text'
--
deletePolicy :: Text -- ^ 'dpPolicyName'
             -> DeletePolicy
deletePolicy p1 = DeletePolicy
    { _dpPolicyName           = p1
    , _dpAutoScalingGroupName = Nothing
    }

-- | The name of the Auto Scaling group.
dpAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dpAutoScalingGroupName =
    lens _dpAutoScalingGroupName (\s a -> s { _dpAutoScalingGroupName = a })

-- | The name or PolicyARN of the policy you want to delete.
dpPolicyName :: Lens' DeletePolicy Text
dpPolicyName = lens _dpPolicyName (\s a -> s { _dpPolicyName = a })

instance ToQuery DeletePolicy

instance ToPath DeletePolicy where
    toPath = const "/"

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePolicyResponse' constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse

instance FromXML DeletePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePolicyResponse"

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = AutoScaling
    type Rs DeletePolicy = DeletePolicyResponse

    request  = post "DeletePolicy"
    response = nullaryResponse DeletePolicyResponse
