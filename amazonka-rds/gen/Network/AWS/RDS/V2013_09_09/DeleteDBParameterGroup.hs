{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified DBParameterGroup. The DBParameterGroup cannot be
-- associated with any RDS instances to be deleted. The specified DB parameter
-- group cannot be associated with any DB instances.
-- https://rds.amazonaws.com/ ?Action=DeleteDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A47%3A08.851Z &AWSAccessKeyId= &Signature=
-- 4dc38be9-bf3b-11de-a88b-7b5b3d23b3a7.
module Network.AWS.RDS.V2013_09_09.DeleteDBParameterGroup
    (
    -- * Request
      DeleteDBParameterGroup
    -- ** Request constructor
    , mkDeleteDBParameterGroup
    -- ** Request lenses
    , ddbpgDBParameterGroupName

    -- * Response
    , DeleteDBParameterGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
newtype DeleteDBParameterGroup = DeleteDBParameterGroup
    { _ddbpgDBParameterGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDBParameterGroup' request.
mkDeleteDBParameterGroup :: Text -- ^ 'ddbpgDBParameterGroupName'
                         -> DeleteDBParameterGroup
mkDeleteDBParameterGroup p1 = DeleteDBParameterGroup
    { _ddbpgDBParameterGroupName = p1
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group You cannot delete a default DB parameter group
-- Cannot be associated with any DB instances.
ddbpgDBParameterGroupName :: Lens' DeleteDBParameterGroup Text
ddbpgDBParameterGroupName =
    lens _ddbpgDBParameterGroupName
         (\s a -> s { _ddbpgDBParameterGroupName = a })

instance ToQuery DeleteDBParameterGroup where
    toQuery = genericQuery def

data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteDBParameterGroup where
    type Sv DeleteDBParameterGroup = RDS
    type Rs DeleteDBParameterGroup = DeleteDBParameterGroupResponse

    request = post "DeleteDBParameterGroup"
    response _ = nullaryResponse DeleteDBParameterGroupResponse
