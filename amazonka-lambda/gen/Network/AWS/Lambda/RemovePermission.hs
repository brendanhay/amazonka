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
-- Revokes function-use permission from an AWS service or another account. You can get the ID of the statement from the output of 'GetPolicy' .
--
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

-- | /See:/ 'removePermission' smart constructor.
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
-- * 'rpQualifier' - Specify a version or alias to remove permissions from a published version of the function.
--
-- * 'rpRevisionId' - Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- * 'rpFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
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


-- | Specify a version or alias to remove permissions from a published version of the function.
rpQualifier :: Lens' RemovePermission (Maybe Text)
rpQualifier = lens _rpQualifier (\ s a -> s{_rpQualifier = a})

-- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
rpRevisionId :: Lens' RemovePermission (Maybe Text)
rpRevisionId = lens _rpRevisionId (\ s a -> s{_rpRevisionId = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
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
