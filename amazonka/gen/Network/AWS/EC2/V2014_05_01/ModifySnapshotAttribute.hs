{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or removes permission settings for the specified snapshot. For more
-- information on modifying snapshot permissions, see Sharing Snapshots in the
-- Amazon Elastic Compute Cloud User Guide. Snapshots with AWS Marketplace
-- product codes cannot be made public. Example This example makes the
-- snap-1a2b3c4d snapshot public, and gives the account with ID 111122223333
-- permission to create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Add.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;
-- Example This example makes the snap-1a2b3c4d snapshot public, and removes
-- the account with ID 111122223333 from the list of users with permission to
-- create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Remove.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;.
module Network.AWS.EC2.V2014_05_01.ModifySnapshotAttribute where

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

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { _msarSnapshotId :: Text
      -- ^ The ID of the snapshot.
    , _msarDryRun :: Bool
      -- ^ 
    , _msarCreateVolumePermission :: CreateVolumePermissionModifications
      -- ^ A JSON representation of the snapshot attribute modification.
    , _msarGroupNames :: [Text]
      -- ^ The group to modify for the snapshot.
    , _msarAttribute :: SnapshotAttributeName
      -- ^ The snapshot attribute to modify.
    , _msarOperationType :: Text
      -- ^ The type of operation to perform to the attribute.
    , _msarUserIds :: [Text]
      -- ^ The account ID to modify for the snapshot.
    } deriving (Generic)

instance ToQuery ModifySnapshotAttribute where
    toQuery = genericToQuery def

instance AWSRequest ModifySnapshotAttribute where
    type Sv ModifySnapshotAttribute = EC2
    type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse

    request = post "ModifySnapshotAttribute"

    response _ _ = return (Right ModifySnapshotAttributeResponse)

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    deriving (Eq, Show, Generic)
