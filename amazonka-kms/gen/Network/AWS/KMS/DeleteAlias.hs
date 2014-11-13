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

-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified alias.
module Network.AWS.KMS.DeleteAlias
    (
    -- * Request
      DeleteAlias
    -- ** Request constructor
    , deleteAlias
    -- ** Request lenses
    , daAliasName

    -- * Response
    , DeleteAliasResponse
    -- ** Response constructor
    , deleteAliasResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

newtype DeleteAlias = DeleteAlias
    { _daAliasName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteAlias' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAliasName' @::@ 'Text'
--
deleteAlias :: Text -- ^ 'daAliasName'
            -> DeleteAlias
deleteAlias p1 = DeleteAlias
    { _daAliasName = p1
    }

-- | The alias to be deleted.
daAliasName :: Lens' DeleteAlias Text
daAliasName = lens _daAliasName (\s a -> s { _daAliasName = a })

instance ToPath DeleteAlias where
    toPath = const "/"

instance ToQuery DeleteAlias where
    toQuery = const mempty

instance ToHeaders DeleteAlias

instance ToBody DeleteAlias where
    toBody = toBody . encode . _daAliasName

data DeleteAliasResponse = DeleteAliasResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAliasResponse' constructor.
deleteAliasResponse :: DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse

-- FromJSON

instance AWSRequest DeleteAlias where
    type Sv DeleteAlias = KMS
    type Rs DeleteAlias = DeleteAliasResponse

    request  = post'
    response = nullaryResponse DeleteAliasResponse
