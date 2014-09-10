{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an existing option group. https://rds.amazonaws.com/
-- ?Action=DeleteOptionGroup &OptionGroupName=myoptiongroup.
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Request
      DeleteOptionGroup
    -- ** Request constructor
    , mkDeleteOptionGroup
    -- ** Request lenses
    , dogOptionGroupName

    -- * Response
    , DeleteOptionGroupResponse
    -- ** Response constructor
    , mkDeleteOptionGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
newtype DeleteOptionGroup = DeleteOptionGroup
    { _dogOptionGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteOptionGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroupName ::@ @Text@
--
mkDeleteOptionGroup :: Text -- ^ 'dogOptionGroupName'
                    -> DeleteOptionGroup
mkDeleteOptionGroup p1 = DeleteOptionGroup
    { _dogOptionGroupName = p1
    }

-- | The name of the option group to be deleted. You cannot delete default
-- option groups.
dogOptionGroupName :: Lens' DeleteOptionGroup Text
dogOptionGroupName =
    lens _dogOptionGroupName (\s a -> s { _dogOptionGroupName = a })

instance ToQuery DeleteOptionGroup where
    toQuery = genericQuery def

data DeleteOptionGroupResponse = DeleteOptionGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteOptionGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteOptionGroupResponse :: DeleteOptionGroupResponse
mkDeleteOptionGroupResponse = DeleteOptionGroupResponse

instance AWSRequest DeleteOptionGroup where
    type Sv DeleteOptionGroup = RDS
    type Rs DeleteOptionGroup = DeleteOptionGroupResponse

    request = post "DeleteOptionGroup"
    response _ = nullaryResponse DeleteOptionGroupResponse
