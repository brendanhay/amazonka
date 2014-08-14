{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a manual snapshot of the specified cluster. The cluster must be in
-- the available state. For more information about working with snapshots, go
-- to Amazon Redshift Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CreateClusterSnapshot
-- &ClusterIdentifier=examplecluster &SnapshotIdentifier=snapshot-1234
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010824Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-123
-- creating manual 1.0 2013-01-23T01:08:29.142Z 2 dev 2013-01-22T19:23:59.368Z
-- us-east-1c dw1.xlarge examplecluster adminuser
-- 65baef14-64f9-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.V2012_12_01.CreateClusterSnapshot where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data CreateClusterSnapshot = CreateClusterSnapshot
    { _ccsmSnapshotIdentifier :: Text
      -- ^ A unique identifier for the snapshot that you are requesting.
      -- This identifier must be unique for all snapshots within the AWS
      -- account. Constraints: Cannot be null, empty, or blank Must
      -- contain from 1 to 255 alphanumeric characters or hyphens First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens Example: my-snapshot-id.
    , _ccsmClusterIdentifier :: Text
      -- ^ The cluster identifier for which you want a snapshot.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSnapshot

instance ToQuery CreateClusterSnapshot where
    toQuery = genericQuery def

data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ssssssssssssssssssssssssssrSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSnapshotResponse

instance FromXML CreateClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterSnapshot where
    type Sv CreateClusterSnapshot = Redshift
    type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse

    request = post "CreateClusterSnapshot"
    response _ = xmlResponse
