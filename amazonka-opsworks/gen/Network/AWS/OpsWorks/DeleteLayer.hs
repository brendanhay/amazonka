{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified layer. You must first stop and then delete all
-- associated instances. For more information, see How to Delete a Layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.DeleteLayer
    (
    -- * Request
      DeleteLayer
    -- ** Request constructor
    , deleteLayer
    -- ** Request lenses
    , dlLayerId

    -- * Response
    , DeleteLayerResponse
    -- ** Response constructor
    , deleteLayerResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteLayer = DeleteLayer
    { _dlLayerId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLayer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LayerId ::@ @Text@
--
deleteLayer :: Text -- ^ 'dlLayerId'
            -> DeleteLayer
deleteLayer p1 = DeleteLayer
    { _dlLayerId = p1
    }

-- | The layer ID.
dlLayerId :: Lens' DeleteLayer Text
dlLayerId = lens _dlLayerId (\s a -> s { _dlLayerId = a })

instance ToPath DeleteLayer

instance ToQuery DeleteLayer

instance ToHeaders DeleteLayer

instance ToJSON DeleteLayer

data DeleteLayerResponse = DeleteLayerResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLayerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteLayerResponse :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse

instance AWSRequest DeleteLayer where
    type Sv DeleteLayer = OpsWorks
    type Rs DeleteLayer = DeleteLayerResponse

    request = get
    response _ = nullaryResponse DeleteLayerResponse
