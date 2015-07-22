{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddPermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to the access policy associated with the specified AWS
-- Lambda function. In a \"push event\" model, the access policy attached
-- to the Lambda function grants Amazon S3 or a user application permission
-- for the Lambda @lambda:Invoke@ action. For information about the push
-- model, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>.
-- Each Lambda function has one access policy associated with it. You can
-- use the @AddPermission@ API to add a permission to the policy. You have
-- one access policy but it can have multiple permission statements.
--
-- This operation requires permission for the @lambda:AddPermission@
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html>
module Network.AWS.Lambda.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , aprqSourceAccount
    , aprqSourceARN
    , aprqFunctionName
    , aprqStatementId
    , aprqAction
    , aprqPrincipal

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    -- ** Response lenses
    , aprsStatement
    , aprsStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprqSourceAccount'
--
-- * 'aprqSourceARN'
--
-- * 'aprqFunctionName'
--
-- * 'aprqStatementId'
--
-- * 'aprqAction'
--
-- * 'aprqPrincipal'
data AddPermission = AddPermission'
    { _aprqSourceAccount :: !(Maybe Text)
    , _aprqSourceARN     :: !(Maybe Text)
    , _aprqFunctionName  :: !Text
    , _aprqStatementId   :: !Text
    , _aprqAction        :: !Text
    , _aprqPrincipal     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddPermission' smart constructor.
addPermission :: Text -> Text -> Text -> Text -> AddPermission
addPermission pFunctionName_ pStatementId_ pAction_ pPrincipal_ =
    AddPermission'
    { _aprqSourceAccount = Nothing
    , _aprqSourceARN = Nothing
    , _aprqFunctionName = pFunctionName_
    , _aprqStatementId = pStatementId_
    , _aprqAction = pAction_
    , _aprqPrincipal = pPrincipal_
    }

-- | The AWS account ID (without a hyphen) of the source owner. For example,
-- if the @SourceArn@ identifies a bucket, then this is the bucket owner\'s
-- account ID. You can use this additional condition to ensure the bucket
-- you specify is owned by a specific account (it is possible the bucket
-- owner deleted the bucket and some other AWS account created the bucket).
-- You can also use this condition to specify all sources (that is, you
-- don\'t specify the @SourceArn@) owned by a specific account.
aprqSourceAccount :: Lens' AddPermission (Maybe Text)
aprqSourceAccount = lens _aprqSourceAccount (\ s a -> s{_aprqSourceAccount = a});

-- | This is optional; however, when granting Amazon S3 permission to invoke
-- your function, you should specify this field with the bucket Amazon
-- Resource Name (ARN) as its value. This ensures that only events
-- generated from the specified bucket can invoke the function.
--
-- If you add a permission for the Amazon S3 principal without providing
-- the source ARN, any AWS account that creates a mapping to your function
-- ARN can send events to invoke your Lambda function from Amazon S3.
aprqSourceARN :: Lens' AddPermission (Maybe Text)
aprqSourceARN = lens _aprqSourceARN (\ s a -> s{_aprqSourceARN = a});

-- | Name of the Lambda function whose access policy you are updating by
-- adding a new permission.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
aprqFunctionName :: Lens' AddPermission Text
aprqFunctionName = lens _aprqFunctionName (\ s a -> s{_aprqFunctionName = a});

-- | A unique statement identifier.
aprqStatementId :: Lens' AddPermission Text
aprqStatementId = lens _aprqStatementId (\ s a -> s{_aprqStatementId = a});

-- | The AWS Lambda action you want to allow in this statement. Each Lambda
-- action is a string starting with \"lambda:\" followed by the API name
-- (see Operations). For example, \"lambda:CreateFunction\". You can use
-- wildcard (\"lambda:*\") to grant permission for all AWS Lambda actions.
aprqAction :: Lens' AddPermission Text
aprqAction = lens _aprqAction (\ s a -> s{_aprqAction = a});

-- | The principal who is getting this permission. It can be Amazon S3
-- service Principal (\"s3.amazonaws.com\") if you want Amazon S3 to invoke
-- the function, an AWS account ID if you are granting cross-account
-- permission, or any valid AWS service principal such as
-- \"sns.amazonaws.com\". For example, you might want to allow a custom
-- application in another AWS account to push events to AWS Lambda by
-- invoking your function.
aprqPrincipal :: Lens' AddPermission Text
aprqPrincipal = lens _aprqPrincipal (\ s a -> s{_aprqPrincipal = a});

instance AWSRequest AddPermission where
        type Sv AddPermission = Lambda
        type Rs AddPermission = AddPermissionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddPermissionResponse' <$>
                   (x .?> "Statement") <*> (pure (fromEnum s)))

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToJSON AddPermission where
        toJSON AddPermission'{..}
          = object
              ["SourceAccount" .= _aprqSourceAccount,
               "SourceArn" .= _aprqSourceARN,
               "StatementId" .= _aprqStatementId,
               "Action" .= _aprqAction,
               "Principal" .= _aprqPrincipal]

instance ToPath AddPermission where
        toPath AddPermission'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _aprqFunctionName,
               "/versions/HEAD/policy"]

instance ToQuery AddPermission where
        toQuery = const mempty

-- | /See:/ 'addPermissionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprsStatement'
--
-- * 'aprsStatus'
data AddPermissionResponse = AddPermissionResponse'
    { _aprsStatement :: !(Maybe Text)
    , _aprsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddPermissionResponse' smart constructor.
addPermissionResponse :: Int -> AddPermissionResponse
addPermissionResponse pStatus_ =
    AddPermissionResponse'
    { _aprsStatement = Nothing
    , _aprsStatus = pStatus_
    }

-- | The permission statement you specified in the request. The response
-- returns the same as a string using \"\\\" as an escape character in the
-- JSON.
aprsStatement :: Lens' AddPermissionResponse (Maybe Text)
aprsStatement = lens _aprsStatement (\ s a -> s{_aprsStatement = a});

-- | FIXME: Undocumented member.
aprsStatus :: Lens' AddPermissionResponse Int
aprsStatus = lens _aprsStatus (\ s a -> s{_aprsStatus = a});
