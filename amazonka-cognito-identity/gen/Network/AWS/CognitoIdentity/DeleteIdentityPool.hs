{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- authenticate with the pool. DeleteIdentityPool The following is an example
-- of a DeleteIdentityPool request. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE" }.
module Network.AWS.CognitoIdentity.DeleteIdentityPool
    (
    -- * Request
      DeleteIdentityPool
    -- ** Request constructor
    , mkDeleteIdentityPool
    -- ** Request lenses
    , dipIdentityPoolId

    -- * Response
    , DeleteIdentityPoolResponse
    -- ** Response constructor
    , mkDeleteIdentityPoolResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the DeleteIdentityPool action.
newtype DeleteIdentityPool = DeleteIdentityPool
    { _dipIdentityPoolId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteIdentityPool' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
mkDeleteIdentityPool :: Text -- ^ 'dipIdentityPoolId'
                     -> DeleteIdentityPool
mkDeleteIdentityPool p1 = DeleteIdentityPool
    { _dipIdentityPoolId = p1
    }

-- | An identity pool ID in the format REGION:GUID.
dipIdentityPoolId :: Lens' DeleteIdentityPool Text
dipIdentityPoolId =
    lens _dipIdentityPoolId (\s a -> s { _dipIdentityPoolId = a })

instance ToPath DeleteIdentityPool

instance ToQuery DeleteIdentityPool

instance ToHeaders DeleteIdentityPool

instance ToJSON DeleteIdentityPool

data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteIdentityPoolResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteIdentityPoolResponse :: DeleteIdentityPoolResponse
mkDeleteIdentityPoolResponse = DeleteIdentityPoolResponse

instance AWSRequest DeleteIdentityPool where
    type Sv DeleteIdentityPool = CognitoIdentity
    type Rs DeleteIdentityPool = DeleteIdentityPoolResponse

    request = get
    response _ = nullaryResponse DeleteIdentityPoolResponse
