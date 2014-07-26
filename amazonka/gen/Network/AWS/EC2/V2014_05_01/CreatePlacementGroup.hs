{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CreatePlacementGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a placement group that you launch cluster instances into. You must
-- give the group a name that's unique within the scope of your account. For
-- more information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example creates a placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=CreatePlacementGroup
-- &amp;GroupName=XYZ-cluster &amp;Strategy=cluster &amp;AUTHPARAMS
-- &lt;CreatePlacementGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/CreatePlacementGroupResponse&gt;.
module Network.AWS.EC2.V2014_05_01.CreatePlacementGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreatePlacementGroup = CreatePlacementGroup
    { _cpgrStrategy :: PlacementStrategy
      -- ^ The placement strategy.
    , _cpgrGroupName :: Text
      -- ^ A name for the placement group. Constraints: Up to 255 ASCII
      -- characters.
    , _cpgrDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery CreatePlacementGroup where
    toQuery = genericToQuery def

instance AWSRequest CreatePlacementGroup where
    type Sv CreatePlacementGroup = EC2
    type Rs CreatePlacementGroup = CreatePlacementGroupResponse

    request = post "CreatePlacementGroup"

    response _ _ = return (Right CreatePlacementGroupResponse)

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    deriving (Eq, Show, Generic)
