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

-- Module      : Network.AWS.Lambda.InvokeAsync
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

-- | <http://docs.aws.amazon.com/lambda/latest/dg/API_InvokeAsync.html>
module Network.AWS.Lambda.InvokeAsync
    (
    -- * Request
      InvokeAsync
    -- ** Request constructor
    , invokeAsync
    -- ** Request lenses
    , iaFunctionName
    , iaInvokeArgs

    -- * Response
    , InvokeAsyncResponse
    -- ** Response constructor
    , invokeAsyncResponse
    -- ** Response lenses
    , iarStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data InvokeAsync = InvokeAsync
    { _iaFunctionName :: Text
    , _iaInvokeArgs   :: RqBody
    } deriving (Show)

-- | 'InvokeAsync' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iaFunctionName' @::@ 'Text'
--
-- * 'iaInvokeArgs' @::@ 'RqBody'
--
invokeAsync :: Text -- ^ 'iaFunctionName'
            -> RqBody -- ^ 'iaInvokeArgs'
            -> InvokeAsync
invokeAsync p1 p2 = InvokeAsync
    { _iaFunctionName = p1
    , _iaInvokeArgs   = p2
    }

iaFunctionName :: Lens' InvokeAsync Text
iaFunctionName = lens _iaFunctionName (\s a -> s { _iaFunctionName = a })

iaInvokeArgs :: Lens' InvokeAsync RqBody
iaInvokeArgs = lens _iaInvokeArgs (\s a -> s { _iaInvokeArgs = a })

newtype InvokeAsyncResponse = InvokeAsyncResponse
    { _iarStatus :: Int
    } deriving (Eq, Ord, Read, Show, Enum, Num, Integral, Real)

-- | 'InvokeAsyncResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iarStatus' @::@ 'Int'
--
invokeAsyncResponse :: Int -- ^ 'iarStatus'
                    -> InvokeAsyncResponse
invokeAsyncResponse p1 = InvokeAsyncResponse
    { _iarStatus = p1
    }

iarStatus :: Lens' InvokeAsyncResponse Int
iarStatus = lens _iarStatus (\s a -> s { _iarStatus = a })

instance ToPath InvokeAsync where
    toPath InvokeAsync{..} = mconcat
        [ "/2014-11-13/functions/"
        , toText _iaFunctionName
        , "/invoke-async/"
        ]

instance ToQuery InvokeAsync where
    toQuery = const mempty

instance ToHeaders InvokeAsync

instance ToBody InvokeAsync where
    toBody = toBody . _iaInvokeArgs

instance AWSRequest InvokeAsync where
    type Sv InvokeAsync = Lambda
    type Rs InvokeAsync = InvokeAsyncResponse

    request  = stream POST
    response = jsonResponse

instance FromJSON InvokeAsyncResponse where
    parseJSON = withObject "InvokeAsyncResponse" $ \o -> InvokeAsyncResponse
        <$> o .:  "Status"
