{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteLayer
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
module Network.AWS.OpsWorks.V2013_02_18.DeleteLayer
    (
    -- * Request
      DeleteLayer
    -- ** Request constructor
    , mkDeleteLayer
    -- ** Request lenses
    , dlLayerId

    -- * Response
    , DeleteLayerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DeleteLayer = DeleteLayer
    { _dlLayerId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLayer' request.
mkDeleteLayer :: Text -- ^ 'dlLayerId'
              -> DeleteLayer
mkDeleteLayer p1 = DeleteLayer
    { _dlLayerId = p1
    }
{-# INLINE mkDeleteLayer #-}

-- | The layer ID.
dlLayerId :: Lens' DeleteLayer Text
dlLayerId = lens _dlLayerId (\s a -> s { _dlLayerId = a })
{-# INLINE dlLayerId #-}

instance ToPath DeleteLayer

instance ToQuery DeleteLayer

instance ToHeaders DeleteLayer

instance ToJSON DeleteLayer

data DeleteLayerResponse = DeleteLayerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLayer where
    type Sv DeleteLayer = OpsWorks
    type Rs DeleteLayer = DeleteLayerResponse

    request = get
    response _ = nullaryResponse DeleteLayerResponse
