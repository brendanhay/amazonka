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

-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified automated cluster snapshot to a new manual cluster
-- snapshot. The source must be an automated snapshot and it must be in the
-- available state. When you delete a cluster, Amazon Redshift deletes any
-- automated snapshots of the cluster. Also, when the retention period of the
-- snapshot expires, Amazon Redshift automatically deletes it. If you want to
-- keep an automated snapshot for a longer period, you can make a manual copy
-- of the snapshot. Manual snapshots are retained until you delete them. For
-- more information about working with snapshots, go to Amazon Redshift
-- Snapshots in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.CopyClusterSnapshot
    (
    -- * Request
      CopyClusterSnapshotMessage
    -- ** Request constructor
    , copyClusterSnapshotMessage
    -- ** Request lenses
    , ccsmSourceSnapshotClusterIdentifier
    , ccsmSourceSnapshotIdentifier
    , ccsmTargetSnapshotIdentifier

    -- * Response
    , CopyClusterSnapshotResult
    -- ** Response constructor
    , copyClusterSnapshotResult
    -- ** Response lenses
    , ccsrSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CopyClusterSnapshotMessage = CopyClusterSnapshotMessage
    { _ccsmSourceSnapshotClusterIdentifier :: Maybe Text
    , _ccsmSourceSnapshotIdentifier        :: Text
    , _ccsmTargetSnapshotIdentifier        :: Text
    } (Eq, Ord, Show, Generic)

-- | 'CopyClusterSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsmSourceSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ccsmSourceSnapshotIdentifier' @::@ 'Text'
--
-- * 'ccsmTargetSnapshotIdentifier' @::@ 'Text'
--
copyClusterSnapshotMessage :: Text -- ^ 'ccsmSourceSnapshotIdentifier'
                           -> Text -- ^ 'ccsmTargetSnapshotIdentifier'
                           -> CopyClusterSnapshotMessage
copyClusterSnapshotMessage p1 p2 = CopyClusterSnapshotMessage
    { _ccsmSourceSnapshotIdentifier        = p1
    , _ccsmTargetSnapshotIdentifier        = p2
    , _ccsmSourceSnapshotClusterIdentifier = Nothing
    }

-- | The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name. Constraints: Must be the identifier for a valid cluster.
ccsmSourceSnapshotClusterIdentifier :: Lens' CopyClusterSnapshotMessage (Maybe Text)
ccsmSourceSnapshotClusterIdentifier =
    lens _ccsmSourceSnapshotClusterIdentifier
        (\s a -> s { _ccsmSourceSnapshotClusterIdentifier = a })

-- | The identifier for the source snapshot. Constraints: Must be the
-- identifier for a valid automated snapshot whose state is available.
ccsmSourceSnapshotIdentifier :: Lens' CopyClusterSnapshotMessage Text
ccsmSourceSnapshotIdentifier =
    lens _ccsmSourceSnapshotIdentifier
        (\s a -> s { _ccsmSourceSnapshotIdentifier = a })

-- | The identifier given to the new manual snapshot. Constraints: Cannot be
-- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for the AWS account that
-- is making the request.
ccsmTargetSnapshotIdentifier :: Lens' CopyClusterSnapshotMessage Text
ccsmTargetSnapshotIdentifier =
    lens _ccsmTargetSnapshotIdentifier
        (\s a -> s { _ccsmTargetSnapshotIdentifier = a })
instance ToQuery CopyClusterSnapshotMessage

instance ToPath CopyClusterSnapshotMessage where
    toPath = const "/"

newtype CopyClusterSnapshotResult = CopyClusterSnapshotResult
    { _ccsrSnapshot :: Maybe Snapshot
    } (Eq, Show, Generic)

-- | 'CopyClusterSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsrSnapshot' @::@ 'Maybe' 'Snapshot'
--
copyClusterSnapshotResult :: CopyClusterSnapshotResult
copyClusterSnapshotResult = CopyClusterSnapshotResult
    { _ccsrSnapshot = Nothing
    }

ccsrSnapshot :: Lens' CopyClusterSnapshotResult (Maybe Snapshot)
ccsrSnapshot = lens _ccsrSnapshot (\s a -> s { _ccsrSnapshot = a })

instance FromXML CopyClusterSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyClusterSnapshotResult"

instance AWSRequest CopyClusterSnapshotMessage where
    type Sv CopyClusterSnapshotMessage = Redshift
    type Rs CopyClusterSnapshotMessage = CopyClusterSnapshotResult

    request  = post "CopyClusterSnapshot"
    response = xmlResponse $ \h x -> CopyClusterSnapshotResult
        <$> x %| "Snapshot"
