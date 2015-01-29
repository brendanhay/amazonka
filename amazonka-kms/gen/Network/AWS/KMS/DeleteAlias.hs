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

-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified alias.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DeleteAlias.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype DeleteAlias = DeleteAlias
    { _daAliasName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

data DeleteAliasResponse = DeleteAliasResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteAliasResponse' constructor.
deleteAliasResponse :: DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse

instance ToPath DeleteAlias where
    toPath = const "/"

instance ToQuery DeleteAlias where
    toQuery = const mempty

instance ToHeaders DeleteAlias

instance ToJSON DeleteAlias where
    toJSON DeleteAlias{..} = object
        [ "AliasName" .= _daAliasName
        ]

instance AWSRequest DeleteAlias where
    type Sv DeleteAlias = KMS
    type Rs DeleteAlias = DeleteAliasResponse

    request  = post "DeleteAlias"
    response = nullResponse DeleteAliasResponse
