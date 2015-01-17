{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Resets permission settings for the specified snapshot.
--
-- For more information on modifying snapshot permissions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetSnapshotAttribute.html>
module Network.AWS.EC2.ResetSnapshotAttribute
    (
    -- * Request
      ResetSnapshotAttribute
    -- ** Request constructor
    , resetSnapshotAttribute
    -- ** Request lenses
    , rsaAttribute
    , rsaDryRun
    , rsaSnapshotId

    -- * Response
    , ResetSnapshotAttributeResponse
    -- ** Response constructor
    , resetSnapshotAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { _rsaAttribute  :: SnapshotAttributeName
    , _rsaDryRun     :: Maybe Bool
    , _rsaSnapshotId :: Text
    } deriving (Eq, Show)

-- | 'ResetSnapshotAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsaAttribute' @::@ 'SnapshotAttributeName'
--
-- * 'rsaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rsaSnapshotId' @::@ 'Text'
--
resetSnapshotAttribute :: Text -- ^ 'rsaSnapshotId'
                       -> SnapshotAttributeName -- ^ 'rsaAttribute'
                       -> ResetSnapshotAttribute
resetSnapshotAttribute p1 p2 = ResetSnapshotAttribute
    { _rsaSnapshotId = p1
    , _rsaAttribute  = p2
    , _rsaDryRun     = Nothing
    }

-- | The attribute to reset (currently only the attribute for permission to create
-- volumes can be reset).
rsaAttribute :: Lens' ResetSnapshotAttribute SnapshotAttributeName
rsaAttribute = lens _rsaAttribute (\s a -> s { _rsaAttribute = a })

rsaDryRun :: Lens' ResetSnapshotAttribute (Maybe Bool)
rsaDryRun = lens _rsaDryRun (\s a -> s { _rsaDryRun = a })

-- | The ID of the snapshot.
rsaSnapshotId :: Lens' ResetSnapshotAttribute Text
rsaSnapshotId = lens _rsaSnapshotId (\s a -> s { _rsaSnapshotId = a })

data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ResetSnapshotAttributeResponse' constructor.
resetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse
resetSnapshotAttributeResponse = ResetSnapshotAttributeResponse

instance ToPath ResetSnapshotAttribute where
    toPath = const "/"

instance ToQuery ResetSnapshotAttribute where
    toQuery ResetSnapshotAttribute{..} = mconcat
        [ "Attribute"  =? _rsaAttribute
        , "dryRun"     =? _rsaDryRun
        , "SnapshotId" =? _rsaSnapshotId
        ]

instance ToHeaders ResetSnapshotAttribute

instance AWSRequest ResetSnapshotAttribute where
    type Sv ResetSnapshotAttribute = EC2
    type Rs ResetSnapshotAttribute = ResetSnapshotAttributeResponse

    request  = post "ResetSnapshotAttribute"
    response = nullResponse ResetSnapshotAttributeResponse
