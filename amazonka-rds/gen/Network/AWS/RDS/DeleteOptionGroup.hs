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

-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an existing option group.
module Network.AWS.RDS.DeleteOptionGroup
    (
    -- * Request
      DeleteOptionGroupMessage
    -- ** Request constructor
    , deleteOptionGroupMessage
    -- ** Request lenses
    , dogmOptionGroupName

    -- * Response
    , DeleteOptionGroupResponse
    -- ** Response constructor
    , deleteOptionGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteOptionGroupMessage = DeleteOptionGroupMessage
    { _dogmOptionGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteOptionGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogmOptionGroupName' @::@ 'Text'
--
deleteOptionGroupMessage :: Text -- ^ 'dogmOptionGroupName'
                         -> DeleteOptionGroupMessage
deleteOptionGroupMessage p1 = DeleteOptionGroupMessage
    { _dogmOptionGroupName = p1
    }

-- | The name of the option group to be deleted.
dogmOptionGroupName :: Lens' DeleteOptionGroupMessage Text
dogmOptionGroupName =
    lens _dogmOptionGroupName (\s a -> s { _dogmOptionGroupName = a })

instance ToPath DeleteOptionGroupMessage where
    toPath = const "/"

instance ToQuery DeleteOptionGroupMessage

data DeleteOptionGroupResponse = DeleteOptionGroupResponse

-- | 'DeleteOptionGroupResponse' constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse

instance AWSRequest DeleteOptionGroupMessage where
    type Sv DeleteOptionGroupMessage = RDS
    type Rs DeleteOptionGroupMessage = DeleteOptionGroupResponse

    request  = post "DeleteOptionGroup"
    response = const (nullaryResponse DeleteOptionGroupResponse)
