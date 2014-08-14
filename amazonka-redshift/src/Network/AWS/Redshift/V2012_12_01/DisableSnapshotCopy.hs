{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DisableSnapshotCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
module Network.AWS.Redshift.V2012_12_01.DisableSnapshotCopy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data DisableSnapshotCopy = DisableSnapshotCopy
    { _dscmClusterIdentifier :: Text
      -- ^ The unique identifier of the source cluster that you want to
      -- disable copying of snapshots to a destination region.
      -- Constraints: Must be the valid name of an existing cluster that
      -- has cross-region snapshot copy enabled.
    } deriving (Show, Generic)

makeLenses ''DisableSnapshotCopy

instance ToQuery DisableSnapshotCopy where
    toQuery = genericQuery def

data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _ccvCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

makeLenses ''DisableSnapshotCopyResponse

instance FromXML DisableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableSnapshotCopy where
    type Sv DisableSnapshotCopy = Redshift
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse

    request = post "DisableSnapshotCopy"
    response _ = xmlResponse
