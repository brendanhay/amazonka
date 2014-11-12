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

-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in the Using IAM
-- guide.
module Network.AWS.IAM.DeleteAccountAlias
    (
    -- * Request
      DeleteAccountAlias
    -- ** Request constructor
    , deleteAccountAlias
    -- ** Request lenses
    , daaAccountAlias

    -- * Response
    , DeleteAccountAliasResponse
    -- ** Response constructor
    , deleteAccountAliasResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype DeleteAccountAlias = DeleteAccountAlias
    { _daaAccountAlias :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteAccountAlias' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAccountAlias' @::@ 'Text'
--
deleteAccountAlias :: Text -- ^ 'daaAccountAlias'
                   -> DeleteAccountAlias
deleteAccountAlias p1 = DeleteAccountAlias
    { _daaAccountAlias = p1
    }

-- | The name of the account alias to delete.
daaAccountAlias :: Lens' DeleteAccountAlias Text
daaAccountAlias = lens _daaAccountAlias (\s a -> s { _daaAccountAlias = a })

instance ToQuery DeleteAccountAlias

instance ToPath DeleteAccountAlias where
    toPath = const "/"

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAccountAliasResponse' constructor.
deleteAccountAliasResponse :: DeleteAccountAliasResponse
deleteAccountAliasResponse = DeleteAccountAliasResponse

instance FromXML DeleteAccountAliasResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAccountAliasResponse"

instance AWSRequest DeleteAccountAlias where
    type Sv DeleteAccountAlias = IAM
    type Rs DeleteAccountAlias = DeleteAccountAliasResponse

    request  = post "DeleteAccountAlias"
    response = nullaryResponse DeleteAccountAliasResponse
