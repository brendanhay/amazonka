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
-- Module      : Network.AWS.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function. To delete a specific function version, use the @Qualifier@ parameter. Otherwise, all versions and aliases are deleted.
--
--
-- To delete Lambda event source mappings that invoke a function, use 'DeleteEventSourceMapping' . For AWS services and resources that invoke your function directly, delete the trigger in the service where you originally configured it.
--
module Network.AWS.Lambda.DeleteFunction
    (
    -- * Creating a Request
      deleteFunction
    , DeleteFunction
    -- * Request Lenses
    , dfQualifier
    , dfFunctionName

    -- * Destructuring the Response
    , deleteFunctionResponse
    , DeleteFunctionResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { _dfQualifier    :: !(Maybe Text)
  , _dfFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfQualifier' - Specify a version to delete. You can't delete a version that's referenced by an alias.
--
-- * 'dfFunctionName' - The name of the Lambda function or version. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
deleteFunction
    :: Text -- ^ 'dfFunctionName'
    -> DeleteFunction
deleteFunction pFunctionName_ =
  DeleteFunction' {_dfQualifier = Nothing, _dfFunctionName = pFunctionName_}


-- | Specify a version to delete. You can't delete a version that's referenced by an alias.
dfQualifier :: Lens' DeleteFunction (Maybe Text)
dfQualifier = lens _dfQualifier (\ s a -> s{_dfQualifier = a})

-- | The name of the Lambda function or version. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
dfFunctionName :: Lens' DeleteFunction Text
dfFunctionName = lens _dfFunctionName (\ s a -> s{_dfFunctionName = a})

instance AWSRequest DeleteFunction where
        type Rs DeleteFunction = DeleteFunctionResponse
        request = delete lambda
        response = receiveNull DeleteFunctionResponse'

instance Hashable DeleteFunction where

instance NFData DeleteFunction where

instance ToHeaders DeleteFunction where
        toHeaders = const mempty

instance ToPath DeleteFunction where
        toPath DeleteFunction'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _dfFunctionName]

instance ToQuery DeleteFunction where
        toQuery DeleteFunction'{..}
          = mconcat ["Qualifier" =: _dfQualifier]

-- | /See:/ 'deleteFunctionResponse' smart constructor.
data DeleteFunctionResponse =
  DeleteFunctionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunctionResponse' with the minimum fields required to make a request.
--
deleteFunctionResponse
    :: DeleteFunctionResponse
deleteFunctionResponse = DeleteFunctionResponse'


instance NFData DeleteFunctionResponse where
