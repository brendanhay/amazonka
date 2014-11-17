{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Lambda function code and configuration. This
-- operation requires permission for the lambda:DeleteFunction action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_DeleteFunction.html>
module Network.AWS.Lambda.DeleteFunction
    (
    -- * Request
      DeleteFunction
    -- ** Request constructor
    , deleteFunction
    -- ** Request lenses
    , dfFunctionName

    -- * Response
    , DeleteFunctionResponse
    -- ** Response constructor
    , deleteFunctionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype DeleteFunction = DeleteFunction
    { _dfFunctionName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteFunction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfFunctionName' @::@ 'Text'
--
deleteFunction :: Text -- ^ 'dfFunctionName'
               -> DeleteFunction
deleteFunction p1 = DeleteFunction
    { _dfFunctionName = p1
    }

-- | The Lambda function to delete.
dfFunctionName :: Lens' DeleteFunction Text
dfFunctionName = lens _dfFunctionName (\s a -> s { _dfFunctionName = a })

data DeleteFunctionResponse = DeleteFunctionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteFunctionResponse' constructor.
deleteFunctionResponse :: DeleteFunctionResponse
deleteFunctionResponse = DeleteFunctionResponse

instance ToPath DeleteFunction where
    toPath DeleteFunction{..} = mconcat
        [ "/2014-11-13/functions/"
        , toText _dfFunctionName
        ]

instance ToQuery DeleteFunction where
    toQuery = const mempty

instance ToHeaders DeleteFunction
instance ToJSON DeleteFunction where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteFunction where
    type Sv DeleteFunction = Lambda
    type Rs DeleteFunction = DeleteFunctionResponse

    request  = delete
    response = nullResponse DeleteFunctionResponse
