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

-- Module      : Network.AWS.Lambda.AddPermission
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

-- | Adds a permission to the access policy associated with the specified AWS
-- Lambda function. In a "push event" model, the access policy attached to the
-- Lambda function grants Amazon S3 or a user application permission for the
-- Lambda 'lambda:Invoke' action. For information about the push model, see <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWSLambda: How it Works>. Each Lambda function has one access policy associated
-- with it. You can use the 'AddPermission' API to add a permission to the policy.
-- You have one access policy but it can have multiple permission statements.
--
-- This operation requires permission for the 'lambda:AddPermission' action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html>
module Network.AWS.Lambda.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apAction
    , apFunctionName
    , apPrincipal
    , apSourceAccount
    , apSourceArn
    , apStatementId

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    -- ** Response lenses
    , aprStatement
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data AddPermission = AddPermission
    { _apAction        :: Text
    , _apFunctionName  :: Text
    , _apPrincipal     :: Text
    , _apSourceAccount :: Maybe Text
    , _apSourceArn     :: Maybe Text
    , _apStatementId   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AddPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apAction' @::@ 'Text'
--
-- * 'apFunctionName' @::@ 'Text'
--
-- * 'apPrincipal' @::@ 'Text'
--
-- * 'apSourceAccount' @::@ 'Maybe' 'Text'
--
-- * 'apSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'apStatementId' @::@ 'Text'
--
addPermission :: Text -- ^ 'apFunctionName'
              -> Text -- ^ 'apStatementId'
              -> Text -- ^ 'apAction'
              -> Text -- ^ 'apPrincipal'
              -> AddPermission
addPermission p1 p2 p3 p4 = AddPermission
    { _apFunctionName  = p1
    , _apStatementId   = p2
    , _apAction        = p3
    , _apPrincipal     = p4
    , _apSourceArn     = Nothing
    , _apSourceAccount = Nothing
    }

-- | The AWS Lambda action you want to allow in this statement. Each Lambda action
-- is a string starting with "lambda:" followed by the API name (see 'Operations'). For example, "lambda:CreateFunction". You can use wildcard ("lambda:*") to grant permission for all AWS Lambda actions.
apAction :: Lens' AddPermission Text
apAction = lens _apAction (\s a -> s { _apAction = a })

-- | Name of the Lambda function whose access policy you are updating by adding a
-- new permission.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
apFunctionName :: Lens' AddPermission Text
apFunctionName = lens _apFunctionName (\s a -> s { _apFunctionName = a })

-- | The principal who is getting this permission. It can be Amazon S3 service
-- Principal ("s3.amazonaws.com") if you want Amazon S3 to invoke the function,
-- an AWS account ID if you are granting cross-account permission, or any valid
-- AWS service principal such as "sns.amazonaws.com". For example, you might
-- want to allow a custom application in another AWS account to push events to
-- AWS Lambda by invoking your function.
apPrincipal :: Lens' AddPermission Text
apPrincipal = lens _apPrincipal (\s a -> s { _apPrincipal = a })

-- | The AWS account ID (without a hyphen) of the source owner. If the 'SourceArn'
-- identifies a bucket, then this is the bucket owner's account ID. You can use
-- this additional condition to ensure the bucket you specify is owned by a
-- specific account (it is possible the bucket owner deleted the bucket and some
-- other AWS account created the bucket). You can also use this condition to
-- specify all sources (that is, you don't specify the 'SourceArn') owned by a
-- specific account.
apSourceAccount :: Lens' AddPermission (Maybe Text)
apSourceAccount = lens _apSourceAccount (\s a -> s { _apSourceAccount = a })

-- | This is optional; however, when granting Amazon S3 permission to invoke your
-- function, you should specify this field with the bucket Amazon Resource Name
-- (ARN) as its value. This ensures that only events generated from the
-- specified bucket can invoke the function.
--
-- If you add a permission for the Amazon S3 principal without providing the
-- source ARN, any AWS account that creates a mapping to your function ARN can
-- send events to invoke your Lambda function from Amazon S3.
apSourceArn :: Lens' AddPermission (Maybe Text)
apSourceArn = lens _apSourceArn (\s a -> s { _apSourceArn = a })

-- | A unique statement identifier.
apStatementId :: Lens' AddPermission Text
apStatementId = lens _apStatementId (\s a -> s { _apStatementId = a })

newtype AddPermissionResponse = AddPermissionResponse
    { _aprStatement :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AddPermissionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprStatement' @::@ 'Maybe' 'Text'
--
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse
    { _aprStatement = Nothing
    }

-- | The permission statement you specified in the request. The response returns
-- the same as a string using "\" as an escape character in the JSON.
aprStatement :: Lens' AddPermissionResponse (Maybe Text)
aprStatement = lens _aprStatement (\s a -> s { _aprStatement = a })

instance ToPath AddPermission where
    toPath AddPermission{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _apFunctionName
        , "/versions/HEAD/policy"
        ]

instance ToQuery AddPermission where
    toQuery = const mempty

instance ToHeaders AddPermission

instance ToJSON AddPermission where
    toJSON AddPermission{..} = object
        [ "StatementId"   .= _apStatementId
        , "Action"        .= _apAction
        , "Principal"     .= _apPrincipal
        , "SourceArn"     .= _apSourceArn
        , "SourceAccount" .= _apSourceAccount
        ]

instance AWSRequest AddPermission where
    type Sv AddPermission = Lambda
    type Rs AddPermission = AddPermissionResponse

    request  = post
    response = jsonResponse

instance FromJSON AddPermissionResponse where
    parseJSON = withObject "AddPermissionResponse" $ \o -> AddPermissionResponse
        <$> o .:? "Statement"
