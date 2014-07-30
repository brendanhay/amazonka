{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified cluster subnet group.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSubnetGroup
-- &ClusterSubnetGroupName=my-subnet-group-2 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T154635Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 3a63806b-6af4-11e2-b27b-4d850b1c672d.
module Network.AWS.Redshift.V2012_12_01.DeleteClusterSubnetGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteClusterSubnetGroup = DeleteClusterSubnetGroup
    { _dcsgmClusterSubnetGroupName :: Text
      -- ^ The name of the cluster subnet group name to be deleted.
    } deriving (Generic)

instance ToQuery DeleteClusterSubnetGroup where
    toQuery = genericToQuery def

instance AWSRequest DeleteClusterSubnetGroup where
    type Sv DeleteClusterSubnetGroup = Redshift
    type Rs DeleteClusterSubnetGroup = DeleteClusterSubnetGroupResponse

    request = post "DeleteClusterSubnetGroup"
    response _ _ = return (Right DeleteClusterSubnetGroupResponse)

data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse
    deriving (Eq, Show, Generic)
