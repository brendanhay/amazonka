{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSecurityGroup &ClusterSecurityGroupName=securitygroup1
-- &Description=my security group &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T005817Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 f9ee270f-64f7-11e2-a8da-655adc216806.
module Network.AWS.Redshift.V2012_12_01.CreateClusterSecurityGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data CreateClusterSecurityGroup = CreateClusterSecurityGroup
    { _ccsgnClusterSecurityGroupName :: Text
      -- ^ The name for the security group. Amazon Redshift stores the value
      -- as a lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Must not be "Default". Must
      -- be unique for all security groups that are created by your AWS
      -- account. Example: examplesecuritygroup.
    , _ccsgnDescription :: Text
      -- ^ A description for the security group.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSecurityGroup

instance ToQuery CreateClusterSecurityGroup where
    toQuery = genericToQuery def

data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse
    { _csgyClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSecurityGroupResponse

instance AWSRequest CreateClusterSecurityGroup where
    type Sv CreateClusterSecurityGroup = Redshift
    type Rs CreateClusterSecurityGroup = CreateClusterSecurityGroupResponse

    request = post "CreateClusterSecurityGroup"
    response _ = cursorResponse $ \hs xml ->
        pure CreateClusterSecurityGroupResponse
            <*> xml %|? "ClusterSecurityGroup"
