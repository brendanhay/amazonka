{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a user pool. Once a pool is deleted, users will not be able to
-- authenticate with the pool.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_DeleteIdentityPool.html>
module Network.AWS.CognitoIdentity.DeleteIdentityPool
    (
    -- * Request
      DeleteIdentityPool
    -- ** Request constructor
    , deleteIdentityPool
    -- ** Request lenses
    , dip1IdentityPoolId

    -- * Response
    , DeleteIdentityPoolResponse
    -- ** Response constructor
    , deleteIdentityPoolResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

newtype DeleteIdentityPool = DeleteIdentityPool
    { _dip1IdentityPoolId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteIdentityPool' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dip1IdentityPoolId' @::@ 'Text'
--
deleteIdentityPool :: Text -- ^ 'dip1IdentityPoolId'
                   -> DeleteIdentityPool
deleteIdentityPool p1 = DeleteIdentityPool
    { _dip1IdentityPoolId = p1
    }

-- | An identity pool ID in the format REGION:GUID.
dip1IdentityPoolId :: Lens' DeleteIdentityPool Text
dip1IdentityPoolId =
    lens _dip1IdentityPoolId (\s a -> s { _dip1IdentityPoolId = a })

data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteIdentityPoolResponse' constructor.
deleteIdentityPoolResponse :: DeleteIdentityPoolResponse
deleteIdentityPoolResponse = DeleteIdentityPoolResponse

instance ToPath DeleteIdentityPool where
    toPath = const "/"

instance ToQuery DeleteIdentityPool where
    toQuery = const mempty

instance ToHeaders DeleteIdentityPool

instance ToJSON DeleteIdentityPool where
    toJSON DeleteIdentityPool{..} = object
        [ "IdentityPoolId" .= _dip1IdentityPoolId
        ]

instance AWSRequest DeleteIdentityPool where
    type Sv DeleteIdentityPool = CognitoIdentity
    type Rs DeleteIdentityPool = DeleteIdentityPoolResponse

    request  = post "DeleteIdentityPool"
    response = nullResponse DeleteIdentityPoolResponse
