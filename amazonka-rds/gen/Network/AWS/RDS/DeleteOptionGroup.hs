{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
      DeleteOptionGroup
    -- ** Request constructor
    , deleteOptionGroup
    -- ** Request lenses
    , dog1OptionGroupName

    -- * Response
    , DeleteOptionGroupResponse
    -- ** Response constructor
    , deleteOptionGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

newtype DeleteOptionGroup = DeleteOptionGroup
    { _dog1OptionGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteOptionGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dog1OptionGroupName' @::@ 'Text'
--
deleteOptionGroup :: Text -- ^ 'dog1OptionGroupName'
                  -> DeleteOptionGroup
deleteOptionGroup p1 = DeleteOptionGroup
    { _dog1OptionGroupName = p1
    }

-- | The name of the option group to be deleted.
dog1OptionGroupName :: Lens' DeleteOptionGroup Text
dog1OptionGroupName =
    lens _dog1OptionGroupName (\s a -> s { _dog1OptionGroupName = a })

instance ToQuery DeleteOptionGroup

instance ToPath DeleteOptionGroup where
    toPath = const "/"

data DeleteOptionGroupResponse = DeleteOptionGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteOptionGroupResponse' constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse

instance AWSRequest DeleteOptionGroup where
    type Sv DeleteOptionGroup = RDS
    type Rs DeleteOptionGroup = DeleteOptionGroupResponse

    request  = post "DeleteOptionGroup"
    response = nullaryResponse DeleteOptionGroupResponse
