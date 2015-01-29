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

-- Module      : Network.AWS.KMS.RetireGrant
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

-- | Retires a grant. You can retire a grant when you're done using it to clean
-- up. You should revoke a grant when you intend to actively deny operations
-- that depend on it.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html>
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
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

newtype RetireGrant = RetireGrant
    { _rgGrantToken :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

data RetireGrantResponse = RetireGrantResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RetireGrantResponse' constructor.
retireGrantResponse :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse

instance ToPath RetireGrant where
    toPath = const "/"

instance ToQuery RetireGrant where
    toQuery = const mempty

instance ToHeaders RetireGrant

instance ToJSON RetireGrant where
    toJSON RetireGrant{..} = object
        [ "GrantToken" .= _rgGrantToken
        ]

instance AWSRequest RetireGrant where
    type Sv RetireGrant = KMS
    type Rs RetireGrant = RetireGrantResponse

    request  = post "RetireGrant"
    response = nullResponse RetireGrantResponse
