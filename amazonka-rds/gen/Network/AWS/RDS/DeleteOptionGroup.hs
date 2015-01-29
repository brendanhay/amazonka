{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteOptionGroup.html>
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
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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
--
-- You cannot delete default option groups.
dog1OptionGroupName :: Lens' DeleteOptionGroup Text
dog1OptionGroupName =
    lens _dog1OptionGroupName (\s a -> s { _dog1OptionGroupName = a })

data DeleteOptionGroupResponse = DeleteOptionGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteOptionGroupResponse' constructor.
deleteOptionGroupResponse :: DeleteOptionGroupResponse
deleteOptionGroupResponse = DeleteOptionGroupResponse

instance ToPath DeleteOptionGroup where
    toPath = const "/"

instance ToQuery DeleteOptionGroup where
    toQuery DeleteOptionGroup{..} = mconcat
        [ "OptionGroupName" =? _dog1OptionGroupName
        ]

instance ToHeaders DeleteOptionGroup

instance AWSRequest DeleteOptionGroup where
    type Sv DeleteOptionGroup = RDS
    type Rs DeleteOptionGroup = DeleteOptionGroupResponse

    request  = post "DeleteOptionGroup"
    response = nullResponse DeleteOptionGroupResponse
