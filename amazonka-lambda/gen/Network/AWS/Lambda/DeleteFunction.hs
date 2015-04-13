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

-- Module      : Network.AWS.Lambda.DeleteFunction
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

-- | Deletes the specified Lambda function code and configuration.
--
-- When you delete a function the associated access policy is also deleted. You
-- will need to delete the event source mappings explicitly.
--
-- This operation requires permission for the 'lambda:DeleteFunction' action.
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype DeleteFunction = DeleteFunction
    { _dfFunctionName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
dfFunctionName :: Lens' DeleteFunction Text
dfFunctionName = lens _dfFunctionName (\s a -> s { _dfFunctionName = a })

data DeleteFunctionResponse = DeleteFunctionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteFunctionResponse' constructor.
deleteFunctionResponse :: DeleteFunctionResponse
deleteFunctionResponse = DeleteFunctionResponse

instance ToPath DeleteFunction where
    toPath DeleteFunction{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _dfFunctionName
        ]

instance ToQuery DeleteFunction where
    toQuery = const mempty

instance ToHeaders DeleteFunction

instance ToJSON DeleteFunction where
    toJSON = const (toJSON Empty)

instance AWSRequest DeleteFunction where
    type Sv DeleteFunction = Lambda
    type Rs DeleteFunction = DeleteFunctionResponse

    request  = delete
    response = nullResponse DeleteFunctionResponse
