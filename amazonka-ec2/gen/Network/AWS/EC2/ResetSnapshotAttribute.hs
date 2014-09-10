{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      ResetSnapshotAttribute
    -- ** Request constructor
    , mkResetSnapshotAttribute
    -- ** Request lenses
    , rsaSnapshotId
    , rsaAttribute

    -- * Response
    , ResetSnapshotAttributeResponse
    -- ** Response constructor
    , mkResetSnapshotAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { _rsaSnapshotId :: !Text
    , _rsaAttribute :: SnapshotAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetSnapshotAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Text@
--
-- * @Attribute ::@ @SnapshotAttributeName@
--
mkResetSnapshotAttribute :: Text -- ^ 'rsaSnapshotId'
                         -> SnapshotAttributeName -- ^ 'rsaAttribute'
                         -> ResetSnapshotAttribute
mkResetSnapshotAttribute p1 p2 = ResetSnapshotAttribute
    { _rsaSnapshotId = p1
    , _rsaAttribute = p2
    }

-- | The ID of the snapshot.
rsaSnapshotId :: Lens' ResetSnapshotAttribute Text
rsaSnapshotId = lens _rsaSnapshotId (\s a -> s { _rsaSnapshotId = a })

-- | The attribute to reset (currently only the attribute for permission to
-- create volumes can be reset).
rsaAttribute :: Lens' ResetSnapshotAttribute SnapshotAttributeName
rsaAttribute = lens _rsaAttribute (\s a -> s { _rsaAttribute = a })

instance ToQuery ResetSnapshotAttribute where
    toQuery = genericQuery def

data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetSnapshotAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkResetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse
mkResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse

instance AWSRequest ResetSnapshotAttribute where
    type Sv ResetSnapshotAttribute = EC2
    type Rs ResetSnapshotAttribute = ResetSnapshotAttributeResponse

    request = post "ResetSnapshotAttribute"
    response _ = nullaryResponse ResetSnapshotAttributeResponse
