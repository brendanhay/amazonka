{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DeleteLayer = DeleteLayer
    { _dlLayerId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteLayer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlLayerId' @::@ 'Text'
--
deleteLayer :: Text -- ^ 'dlLayerId'
            -> DeleteLayer
deleteLayer p1 = DeleteLayer
    { _dlLayerId = p1
    }

-- | The layer ID.
dlLayerId :: Lens' DeleteLayer Text
dlLayerId = lens _dlLayerId (\s a -> s { _dlLayerId = a })

instance ToPath DeleteLayer where
    toPath = const "/"

instance ToQuery DeleteLayer where
    toQuery = const mempty

instance ToHeaders DeleteLayer

instance ToBody DeleteLayer where
    toBody = toBody . encode . _dlLayerId

data DeleteLayerResponse = DeleteLayerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLayerResponse' constructor.
deleteLayerResponse :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse

instance AWSRequest DeleteLayer where
    type Sv DeleteLayer = OpsWorks
    type Rs DeleteLayer = DeleteLayerResponse

    request  = post
    response = nullaryResponse DeleteLayerResponse
