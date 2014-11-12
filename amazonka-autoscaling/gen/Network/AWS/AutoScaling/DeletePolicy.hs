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
      DeletePolicyType
    -- ** Request constructor
    , deletePolicyType
    -- ** Request lenses
    , dpt1AutoScalingGroupName
    , dpt1PolicyName

    -- * Response
    , DeletePolicyResponse
    -- ** Response constructor
    , deletePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeletePolicyType = DeletePolicyType
    { _dpt1AutoScalingGroupName :: Maybe Text
    , _dpt1PolicyName           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeletePolicyType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpt1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dpt1PolicyName' @::@ 'Text'
--
deletePolicyType :: Text -- ^ 'dpt1PolicyName'
                 -> DeletePolicyType
deletePolicyType p1 = DeletePolicyType
    { _dpt1PolicyName           = p1
    , _dpt1AutoScalingGroupName = Nothing
    }

-- | The name of the Auto Scaling group.
dpt1AutoScalingGroupName :: Lens' DeletePolicyType (Maybe Text)
dpt1AutoScalingGroupName =
    lens _dpt1AutoScalingGroupName
        (\s a -> s { _dpt1AutoScalingGroupName = a })

-- | The name or PolicyARN of the policy you want to delete.
dpt1PolicyName :: Lens' DeletePolicyType Text
dpt1PolicyName = lens _dpt1PolicyName (\s a -> s { _dpt1PolicyName = a })

instance ToQuery DeletePolicyType

instance ToPath DeletePolicyType where
    toPath = const "/"

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePolicyResponse' constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse

instance FromXML DeletePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePolicyResponse"

instance AWSRequest DeletePolicyType where
    type Sv DeletePolicyType = AutoScaling
    type Rs DeletePolicyType = DeletePolicyResponse

    request  = post "DeletePolicy"
    response = nullaryResponse DeletePolicyResponse
