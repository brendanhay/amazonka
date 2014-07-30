{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified Amazon Redshift parameter group. You cannot delete a
-- parameter group if it is associated with a cluster.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DeleteClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T015410Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 29674ca0-40da-11e2-b679-dba6cf515770.
module Network.AWS.Redshift.V2012_12_01.DeleteClusterParameterGroup where

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

data DeleteClusterParameterGroup = DeleteClusterParameterGroup
    { _dcpgnParameterGroupName :: Text
      -- ^ The name of the parameter group to be deleted. Constraints: Must
      -- be the name of an existing cluster parameter group. Cannot delete
      -- a default cluster parameter group.
    } deriving (Generic)

instance ToQuery DeleteClusterParameterGroup where
    toQuery = genericToQuery def

instance AWSRequest DeleteClusterParameterGroup where
    type Sv DeleteClusterParameterGroup = Redshift
    type Rs DeleteClusterParameterGroup = DeleteClusterParameterGroupResponse

    request = post "DeleteClusterParameterGroup"
    response _ _ = return (Right DeleteClusterParameterGroupResponse)

data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse
    deriving (Eq, Show, Generic)
