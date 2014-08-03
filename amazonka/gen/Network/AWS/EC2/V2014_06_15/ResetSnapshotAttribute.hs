{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets permission settings for the specified snapshot. For more information
-- on modifying snapshot permissions, see Sharing Snapshots in the Amazon
-- Elastic Compute Cloud User Guide. Example This example resets the
-- permissions for snap-1a2b3c4d, making it a private snapshot that can only
-- be used by the account that created it.
-- https://ec2.amazonaws.com/?Action=ResetSnapshotAttribute
-- &amp;SnapshotId=snap-1a2b3c4d &amp;Attribute=createVolumePermission
-- &amp;AUTHPARAMS &lt;ResetSnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetSnapshotAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ResetSnapshotAttribute where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ResetSnapshotAttribute' request.
resetSnapshotAttribute :: SnapshotAttributeName -- ^ '_rsarAttribute'
                       -> Text -- ^ '_rsarSnapshotId'
                       -> ResetSnapshotAttribute
resetSnapshotAttribute p1 p2 = ResetSnapshotAttribute
    { _rsarAttribute = p1
    , _rsarSnapshotId = p2
    , _rsarDryRun = Nothing
    }

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { _rsarAttribute :: SnapshotAttributeName
      -- ^ The attribute to reset (currently only the attribute for
      -- permission to create volumes can be reset).
    , _rsarSnapshotId :: Text
      -- ^ The ID of the snapshot.
    , _rsarDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''ResetSnapshotAttribute

instance ToQuery ResetSnapshotAttribute where
    toQuery = genericToQuery def

data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    deriving (Eq, Show, Generic)

makeLenses ''ResetSnapshotAttributeResponse

instance AWSRequest ResetSnapshotAttribute where
    type Sv ResetSnapshotAttribute = EC2
    type Rs ResetSnapshotAttribute = ResetSnapshotAttributeResponse

    request = post "ResetSnapshotAttribute"
    response _ _ = return (Right ResetSnapshotAttributeResponse)
