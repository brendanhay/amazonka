{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB subnet group. The specified database subnet group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSubnetGroup &DBSubnetGroupName=mysubnetgroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.RDS.V2013_09_09.DeleteDBSubnetGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

data DeleteDBSubnetGroup = DeleteDBSubnetGroup
    { _ddbsgnDBSubnetGroupName :: Text
      -- ^ The name of the database subnet group to delete. You cannot
      -- delete the default subnet group. Constraints: Must be 1 to 255
      -- alphanumeric characters First character must be a letter Cannot
      -- end with a hyphen or contain two consecutive hyphens.
    } deriving (Show, Generic)

makeLenses ''DeleteDBSubnetGroup

instance ToQuery DeleteDBSubnetGroup where
    toQuery = genericQuery def

data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteDBSubnetGroupResponse

instance AWSRequest DeleteDBSubnetGroup where
    type Sv DeleteDBSubnetGroup = RDS
    type Rs DeleteDBSubnetGroup = DeleteDBSubnetGroupResponse

    request = post "DeleteDBSubnetGroup"
    response _ = nullaryResponse DeleteDBSubnetGroupResponse
