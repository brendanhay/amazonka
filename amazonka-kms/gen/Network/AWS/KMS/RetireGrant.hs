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

-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retires a grant. You can retire a grant when you're done using it to clean
-- up. You should revoke a grant when you intend to actively deny operations
-- that depend on it.
module Network.AWS.KMS.RetireGrant
    (
    -- * Request
      RetireGrant
    -- ** Request constructor
    , retireGrant
    -- ** Request lenses
    , rgGrantToken

    -- * Response
    , RetireGrantResponse
    -- ** Response constructor
    , retireGrantResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

newtype RetireGrant = RetireGrant
    { _rgGrantToken :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'RetireGrant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgGrantToken' @::@ 'Text'
--
retireGrant :: Text -- ^ 'rgGrantToken'
            -> RetireGrant
retireGrant p1 = RetireGrant
    { _rgGrantToken = p1
    }

-- | Token that identifies the grant to be retired.
rgGrantToken :: Lens' RetireGrant Text
rgGrantToken = lens _rgGrantToken (\s a -> s { _rgGrantToken = a })

instance ToPath RetireGrant where
    toPath = const "/"

instance ToQuery RetireGrant where
    toQuery = const mempty

instance ToHeaders RetireGrant

instance ToBody RetireGrant where
    toBody = toBody . encode . _rgGrantToken

data RetireGrantResponse = RetireGrantResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RetireGrantResponse' constructor.
retireGrantResponse :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse

instance AWSRequest RetireGrant where
    type Sv RetireGrant = KMS
    type Rs RetireGrant = RetireGrantResponse

    request  = post
    response = nullaryResponse RetireGrantResponse
