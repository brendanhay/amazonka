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

-- Module      : Network.AWS.Lambda.RemovePermission
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

-- | You can remove individual permissions from an access policy associated with a
-- Lambda function by providing a Statement ID.
--
-- Note that removal of a permission will cause an active event source to lose
-- permission to the function.
--
-- You need permission for the 'lambda:RemovePermission' action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_RemovePermission.html>
module Network.AWS.Lambda.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , removePermission
    -- ** Request lenses
    , rpFunctionName
    , rpStatementId

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data RemovePermission = RemovePermission
    { _rpFunctionName :: Text
    , _rpStatementId  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RemovePermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpFunctionName' @::@ 'Text'
--
-- * 'rpStatementId' @::@ 'Text'
--
removePermission :: Text -- ^ 'rpFunctionName'
                 -> Text -- ^ 'rpStatementId'
                 -> RemovePermission
removePermission p1 p2 = RemovePermission
    { _rpFunctionName = p1
    , _rpStatementId  = p2
    }

-- | Lambda function whose access policy you want to remove a permission from.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
rpFunctionName :: Lens' RemovePermission Text
rpFunctionName = lens _rpFunctionName (\s a -> s { _rpFunctionName = a })

-- | Statement ID of the permission to remove.
rpStatementId :: Lens' RemovePermission Text
rpStatementId = lens _rpStatementId (\s a -> s { _rpStatementId = a })

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RemovePermissionResponse' constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse

instance ToPath RemovePermission where
    toPath RemovePermission{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _rpFunctionName
        , "/versions/HEAD/policy/"
        , toText _rpStatementId
        ]

instance ToQuery RemovePermission where
    toQuery = const mempty

instance ToHeaders RemovePermission

instance ToJSON RemovePermission where
    toJSON = const (toJSON Empty)

instance AWSRequest RemovePermission where
    type Sv RemovePermission = Lambda
    type Rs RemovePermission = RemovePermissionResponse

    request  = delete
    response = nullResponse RemovePermissionResponse
