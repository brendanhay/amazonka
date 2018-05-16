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
-- Module      : Network.AWS.Lambda.RemovePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can remove individual permissions from an resource policy associated with a Lambda function by providing a statement ID that you provided when you added the permission.
--
--
-- If you are using versioning, the permissions you remove are specific to the Lambda function version or alias you specify in the @AddPermission@ request via the @Qualifier@ parameter. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- Note that removal of a permission will cause an active event source to lose permission to the function.
--
-- You need permission for the @lambda:RemovePermission@ action.
--
module Network.AWS.Lambda.RemovePermission
    (
    -- * Creating a Request
      removePermission
    , RemovePermission
    -- * Request Lenses
    , rpQualifier
    , rpRevisionId
    , rpFunctionName
    , rpStatementId

    -- * Destructuring the Response
    , removePermissionResponse
    , RemovePermissionResponse
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
-- /See:/ 'removePermission' smart constructor.
data RemovePermission = RemovePermission'
  { _rpQualifier    :: !(Maybe Text)
  , _rpRevisionId   :: !(Maybe Text)
  , _rpFunctionName :: !Text
  , _rpStatementId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpQualifier' - You can specify this optional parameter to remove permission associated with a specific function version or function alias. If you don't specify this parameter, the API removes permission associated with the unqualified function ARN.
--
-- * 'rpRevisionId' - An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
--
-- * 'rpFunctionName' - Lambda function whose resource policy you want to remove a permission from. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'rpStatementId' - Statement ID of the permission to remove.
removePermission
    :: Text -- ^ 'rpFunctionName'
    -> Text -- ^ 'rpStatementId'
    -> RemovePermission
removePermission pFunctionName_ pStatementId_ =
  RemovePermission'
    { _rpQualifier = Nothing
    , _rpRevisionId = Nothing
    , _rpFunctionName = pFunctionName_
    , _rpStatementId = pStatementId_
    }


-- | You can specify this optional parameter to remove permission associated with a specific function version or function alias. If you don't specify this parameter, the API removes permission associated with the unqualified function ARN.
rpQualifier :: Lens' RemovePermission (Maybe Text)
rpQualifier = lens _rpQualifier (\ s a -> s{_rpQualifier = a})

-- | An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
rpRevisionId :: Lens' RemovePermission (Maybe Text)
rpRevisionId = lens _rpRevisionId (\ s a -> s{_rpRevisionId = a})

-- | Lambda function whose resource policy you want to remove a permission from. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
rpFunctionName :: Lens' RemovePermission Text
rpFunctionName = lens _rpFunctionName (\ s a -> s{_rpFunctionName = a})

-- | Statement ID of the permission to remove.
rpStatementId :: Lens' RemovePermission Text
rpStatementId = lens _rpStatementId (\ s a -> s{_rpStatementId = a})

instance AWSRequest RemovePermission where
        type Rs RemovePermission = RemovePermissionResponse
        request = delete lambda
        response = receiveNull RemovePermissionResponse'

instance Hashable RemovePermission where

instance NFData RemovePermission where

instance ToHeaders RemovePermission where
        toHeaders = const mempty

instance ToPath RemovePermission where
        toPath RemovePermission'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _rpFunctionName,
               "/policy/", toBS _rpStatementId]

instance ToQuery RemovePermission where
        toQuery RemovePermission'{..}
          = mconcat
              ["Qualifier" =: _rpQualifier,
               "RevisionId" =: _rpRevisionId]

-- | /See:/ 'removePermissionResponse' smart constructor.
data RemovePermissionResponse =
  RemovePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemovePermissionResponse' with the minimum fields required to make a request.
--
removePermissionResponse
    :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse'


instance NFData RemovePermissionResponse where
