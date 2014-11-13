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

-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes a grant. You can revoke a grant to actively deny operations that
-- depend on it.
module Network.AWS.KMS.RevokeGrant
    (
    -- * Request
      RevokeGrant
    -- ** Request constructor
    , revokeGrant
    -- ** Request lenses
    , rgGrantId
    , rgKeyId

    -- * Response
    , RevokeGrantResponse
    -- ** Response constructor
    , revokeGrantResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

data RevokeGrant = RevokeGrant
    { _rgGrantId :: Text
    , _rgKeyId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeGrant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgGrantId' @::@ 'Text'
--
-- * 'rgKeyId' @::@ 'Text'
--
revokeGrant :: Text -- ^ 'rgKeyId'
            -> Text -- ^ 'rgGrantId'
            -> RevokeGrant
revokeGrant p1 p2 = RevokeGrant
    { _rgKeyId   = p1
    , _rgGrantId = p2
    }

-- | Identifier of the grant to be revoked.
rgGrantId :: Lens' RevokeGrant Text
rgGrantId = lens _rgGrantId (\s a -> s { _rgGrantId = a })

-- | Unique identifier of the key associated with the grant.
rgKeyId :: Lens' RevokeGrant Text
rgKeyId = lens _rgKeyId (\s a -> s { _rgKeyId = a })

instance ToPath RevokeGrant where
    toPath = const "/"

instance ToQuery RevokeGrant where
    toQuery = const mempty

instance ToHeaders RevokeGrant

instance ToBody RevokeGrant where
    toBody = toBody . encode . _rgKeyId

data RevokeGrantResponse = RevokeGrantResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RevokeGrantResponse' constructor.
revokeGrantResponse :: RevokeGrantResponse
revokeGrantResponse = RevokeGrantResponse

-- FromJSON

instance AWSRequest RevokeGrant where
    type Sv RevokeGrant = KMS
    type Rs RevokeGrant = RevokeGrantResponse

    request  = post'
    response = nullaryResponse RevokeGrantResponse
