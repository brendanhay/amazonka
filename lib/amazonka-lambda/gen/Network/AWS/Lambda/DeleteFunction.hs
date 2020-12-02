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
-- Deletes the specified Lambda function code and configuration.
--
--
-- If you are using the versioning feature and you don't specify a function version in your @DeleteFunction@ request, AWS Lambda will delete the function, including all its versions, and any aliases pointing to the function versions. To delete a specific function version, you must provide the function version via the @Qualifier@ parameter. For information about function versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- When you delete a function the associated resource policy is also deleted. You will need to delete the event source mappings explicitly.
--
-- This operation requires permission for the @lambda:DeleteFunction@ action.
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
-- * 'dfQualifier' - Using this optional parameter you can specify a function version (but not the @> LATEST@ version) to direct AWS Lambda to delete a specific function version. If the function version has one or more aliases pointing to it, you will get an error because you cannot have aliases pointing to it. You can delete any function version but not the @> LATEST@ , that is, you cannot specify @> LATEST@ as the value of this parameter. The @> LATEST@ version can be deleted only when you want to delete all the function versions and aliases. You can only specify a function version, not an alias name, using this parameter. You cannot delete a function version using its alias. If you don't specify this parameter, AWS Lambda will delete the function, including all of its versions and aliases.
--
-- * 'dfFunctionName' - The Lambda function to delete. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
deleteFunction
    :: Text -- ^ 'dfFunctionName'
    -> DeleteFunction
deleteFunction pFunctionName_ =
  DeleteFunction' {_dfQualifier = Nothing, _dfFunctionName = pFunctionName_}


-- | Using this optional parameter you can specify a function version (but not the @> LATEST@ version) to direct AWS Lambda to delete a specific function version. If the function version has one or more aliases pointing to it, you will get an error because you cannot have aliases pointing to it. You can delete any function version but not the @> LATEST@ , that is, you cannot specify @> LATEST@ as the value of this parameter. The @> LATEST@ version can be deleted only when you want to delete all the function versions and aliases. You can only specify a function version, not an alias name, using this parameter. You cannot delete a function version using its alias. If you don't specify this parameter, AWS Lambda will delete the function, including all of its versions and aliases.
dfQualifier :: Lens' DeleteFunction (Maybe Text)
dfQualifier = lens _dfQualifier (\ s a -> s{_dfQualifier = a})

-- | The Lambda function to delete. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
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
