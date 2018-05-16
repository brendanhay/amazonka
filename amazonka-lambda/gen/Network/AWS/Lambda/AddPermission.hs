{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to the resource policy associated with the specified AWS Lambda function. You use resource policies to grant permissions to event sources that use /push/ model. In a /push/ model, event sources (such as Amazon S3 and custom applications) invoke your Lambda function. Each permission you add to the resource policy allows an event source, permission to invoke the Lambda function.
--
--
-- For information about the push model, see <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html Lambda Functions> .
--
-- If you are using versioning, the permissions you add are specific to the Lambda function version or alias you specify in the @AddPermission@ request via the @Qualifier@ parameter. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:AddPermission@ action.
--
module Network.AWS.Lambda.AddPermission
    (
    -- * Creating a Request
      addPermission
    , AddPermission
    -- * Request Lenses
    , apSourceAccount
    , apEventSourceToken
    , apSourceARN
    , apQualifier
    , apRevisionId
    , apFunctionName
    , apStatementId
    , apAction
    , apPrincipal

    -- * Destructuring the Response
    , addPermissionResponse
    , AddPermissionResponse
    -- * Response Lenses
    , aprsStatement
    , aprsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'addPermission' smart constructor.
data AddPermission = AddPermission'
  { _apSourceAccount    :: !(Maybe Text)
  , _apEventSourceToken :: !(Maybe Text)
  , _apSourceARN        :: !(Maybe Text)
  , _apQualifier        :: !(Maybe Text)
  , _apRevisionId       :: !(Maybe Text)
  , _apFunctionName     :: !Text
  , _apStatementId      :: !Text
  , _apAction           :: !Text
  , _apPrincipal        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apSourceAccount' - This parameter is used for S3 and SES. The AWS account ID (without a hyphen) of the source owner. For example, if the @SourceArn@ identifies a bucket, then this is the bucket owner's account ID. You can use this additional condition to ensure the bucket you specify is owned by a specific account (it is possible the bucket owner deleted the bucket and some other AWS account created the bucket). You can also use this condition to specify all sources (that is, you don't specify the @SourceArn@ ) owned by a specific account.
--
-- * 'apEventSourceToken' - A unique token that must be supplied by the principal invoking the function. This is currently only used for Alexa Smart Home functions.
--
-- * 'apSourceARN' - This is optional; however, when granting permission to invoke your function, you should specify this field with the Amazon Resource Name (ARN) as its value. This ensures that only events generated from the specified source can invoke the function. /Important:/ If you add a permission without providing the source ARN, any AWS account that creates a mapping to your function ARN can send events to invoke your Lambda function.
--
-- * 'apQualifier' - You can use this optional query parameter to describe a qualified ARN using a function version or an alias name. The permission will then apply to the specific qualified ARN. For example, if you specify function version 2 as the qualifier, then permission applies only when request is made using qualified function ARN: @arn:aws:lambda:aws-region:acct-id:function:function-name:2@  If you specify an alias name, for example @PROD@ , then the permission is valid only for requests made using the alias ARN: @arn:aws:lambda:aws-region:acct-id:function:function-name:PROD@  If the qualifier is not specified, the permission is valid only when requests is made using unqualified function ARN. @arn:aws:lambda:aws-region:acct-id:function:function-name@
--
-- * 'apRevisionId' - An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
--
-- * 'apFunctionName' - Name of the Lambda function whose resource policy you are updating by adding a new permission. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'apStatementId' - A unique statement identifier.
--
-- * 'apAction' - The AWS Lambda action you want to allow in this statement. Each Lambda action is a string starting with @lambda:@ followed by the API name . For example, @lambda:CreateFunction@ . You can use wildcard (@lambda:*@ ) to grant permission for all AWS Lambda actions.
--
-- * 'apPrincipal' - The principal who is getting this permission. It can be Amazon S3 service Principal (@s3.amazonaws.com@ ) if you want Amazon S3 to invoke the function, an AWS account ID if you are granting cross-account permission, or any valid AWS service principal such as @sns.amazonaws.com@ . For example, you might want to allow a custom application in another AWS account to push events to AWS Lambda by invoking your function.
addPermission
    :: Text -- ^ 'apFunctionName'
    -> Text -- ^ 'apStatementId'
    -> Text -- ^ 'apAction'
    -> Text -- ^ 'apPrincipal'
    -> AddPermission
addPermission pFunctionName_ pStatementId_ pAction_ pPrincipal_ =
  AddPermission'
    { _apSourceAccount = Nothing
    , _apEventSourceToken = Nothing
    , _apSourceARN = Nothing
    , _apQualifier = Nothing
    , _apRevisionId = Nothing
    , _apFunctionName = pFunctionName_
    , _apStatementId = pStatementId_
    , _apAction = pAction_
    , _apPrincipal = pPrincipal_
    }


-- | This parameter is used for S3 and SES. The AWS account ID (without a hyphen) of the source owner. For example, if the @SourceArn@ identifies a bucket, then this is the bucket owner's account ID. You can use this additional condition to ensure the bucket you specify is owned by a specific account (it is possible the bucket owner deleted the bucket and some other AWS account created the bucket). You can also use this condition to specify all sources (that is, you don't specify the @SourceArn@ ) owned by a specific account.
apSourceAccount :: Lens' AddPermission (Maybe Text)
apSourceAccount = lens _apSourceAccount (\ s a -> s{_apSourceAccount = a})

-- | A unique token that must be supplied by the principal invoking the function. This is currently only used for Alexa Smart Home functions.
apEventSourceToken :: Lens' AddPermission (Maybe Text)
apEventSourceToken = lens _apEventSourceToken (\ s a -> s{_apEventSourceToken = a})

-- | This is optional; however, when granting permission to invoke your function, you should specify this field with the Amazon Resource Name (ARN) as its value. This ensures that only events generated from the specified source can invoke the function. /Important:/ If you add a permission without providing the source ARN, any AWS account that creates a mapping to your function ARN can send events to invoke your Lambda function.
apSourceARN :: Lens' AddPermission (Maybe Text)
apSourceARN = lens _apSourceARN (\ s a -> s{_apSourceARN = a})

-- | You can use this optional query parameter to describe a qualified ARN using a function version or an alias name. The permission will then apply to the specific qualified ARN. For example, if you specify function version 2 as the qualifier, then permission applies only when request is made using qualified function ARN: @arn:aws:lambda:aws-region:acct-id:function:function-name:2@  If you specify an alias name, for example @PROD@ , then the permission is valid only for requests made using the alias ARN: @arn:aws:lambda:aws-region:acct-id:function:function-name:PROD@  If the qualifier is not specified, the permission is valid only when requests is made using unqualified function ARN. @arn:aws:lambda:aws-region:acct-id:function:function-name@
apQualifier :: Lens' AddPermission (Maybe Text)
apQualifier = lens _apQualifier (\ s a -> s{_apQualifier = a})

-- | An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
apRevisionId :: Lens' AddPermission (Maybe Text)
apRevisionId = lens _apRevisionId (\ s a -> s{_apRevisionId = a})

-- | Name of the Lambda function whose resource policy you are updating by adding a new permission. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
apFunctionName :: Lens' AddPermission Text
apFunctionName = lens _apFunctionName (\ s a -> s{_apFunctionName = a})

-- | A unique statement identifier.
apStatementId :: Lens' AddPermission Text
apStatementId = lens _apStatementId (\ s a -> s{_apStatementId = a})

-- | The AWS Lambda action you want to allow in this statement. Each Lambda action is a string starting with @lambda:@ followed by the API name . For example, @lambda:CreateFunction@ . You can use wildcard (@lambda:*@ ) to grant permission for all AWS Lambda actions.
apAction :: Lens' AddPermission Text
apAction = lens _apAction (\ s a -> s{_apAction = a})

-- | The principal who is getting this permission. It can be Amazon S3 service Principal (@s3.amazonaws.com@ ) if you want Amazon S3 to invoke the function, an AWS account ID if you are granting cross-account permission, or any valid AWS service principal such as @sns.amazonaws.com@ . For example, you might want to allow a custom application in another AWS account to push events to AWS Lambda by invoking your function.
apPrincipal :: Lens' AddPermission Text
apPrincipal = lens _apPrincipal (\ s a -> s{_apPrincipal = a})

instance AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        request = postJSON lambda
        response
          = receiveJSON
              (\ s h x ->
                 AddPermissionResponse' <$>
                   (x .?> "Statement") <*> (pure (fromEnum s)))

instance Hashable AddPermission where

instance NFData AddPermission where

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToJSON AddPermission where
        toJSON AddPermission'{..}
          = object
              (catMaybes
                 [("SourceAccount" .=) <$> _apSourceAccount,
                  ("EventSourceToken" .=) <$> _apEventSourceToken,
                  ("SourceArn" .=) <$> _apSourceARN,
                  ("RevisionId" .=) <$> _apRevisionId,
                  Just ("StatementId" .= _apStatementId),
                  Just ("Action" .= _apAction),
                  Just ("Principal" .= _apPrincipal)])

instance ToPath AddPermission where
        toPath AddPermission'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _apFunctionName,
               "/policy"]

instance ToQuery AddPermission where
        toQuery AddPermission'{..}
          = mconcat ["Qualifier" =: _apQualifier]

-- |
--
--
--
-- /See:/ 'addPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  { _aprsStatement      :: !(Maybe Text)
  , _aprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aprsStatement' - The permission statement you specified in the request. The response returns the same as a string using a backslash ("\") as an escape character in the JSON.
--
-- * 'aprsResponseStatus' - -- | The response status code.
addPermissionResponse
    :: Int -- ^ 'aprsResponseStatus'
    -> AddPermissionResponse
addPermissionResponse pResponseStatus_ =
  AddPermissionResponse'
    {_aprsStatement = Nothing, _aprsResponseStatus = pResponseStatus_}


-- | The permission statement you specified in the request. The response returns the same as a string using a backslash ("\") as an escape character in the JSON.
aprsStatement :: Lens' AddPermissionResponse (Maybe Text)
aprsStatement = lens _aprsStatement (\ s a -> s{_aprsStatement = a})

-- | -- | The response status code.
aprsResponseStatus :: Lens' AddPermissionResponse Int
aprsResponseStatus = lens _aprsResponseStatus (\ s a -> s{_aprsResponseStatus = a})

instance NFData AddPermissionResponse where
