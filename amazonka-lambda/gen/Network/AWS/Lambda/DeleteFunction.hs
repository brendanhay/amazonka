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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Lambda function code and configuration.
--
-- When you delete a function the associated access policy is also deleted.
-- You will need to delete the event source mappings explicitly.
--
-- This operation requires permission for the 'lambda:DeleteFunction'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_DeleteFunction.html AWS API Reference> for DeleteFunction.
module Network.AWS.Lambda.DeleteFunction
    (
    -- * Creating a Request
      deleteFunction
    , DeleteFunction
    -- * Request Lenses
    , dfFunctionName

    -- * Destructuring the Response
    , deleteFunctionResponse
    , DeleteFunctionResponse
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFunction' smart constructor.
newtype DeleteFunction = DeleteFunction'
    { _dfFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfFunctionName'
deleteFunction
    :: Text -- ^ 'dfFunctionName'
    -> DeleteFunction
deleteFunction pFunctionName_ =
    DeleteFunction'
    { _dfFunctionName = pFunctionName_
    }

-- | The Lambda function to delete.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
dfFunctionName :: Lens' DeleteFunction Text
dfFunctionName = lens _dfFunctionName (\ s a -> s{_dfFunctionName = a});

instance AWSRequest DeleteFunction where
        type Sv DeleteFunction = Lambda
        type Rs DeleteFunction = DeleteFunctionResponse
        request = delete
        response = receiveNull DeleteFunctionResponse'

instance ToHeaders DeleteFunction where
        toHeaders = const mempty

instance ToPath DeleteFunction where
        toPath DeleteFunction'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _dfFunctionName]

instance ToQuery DeleteFunction where
        toQuery = const mempty

-- | /See:/ 'deleteFunctionResponse' smart constructor.
data DeleteFunctionResponse =
    DeleteFunctionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFunctionResponse' with the minimum fields required to make a request.
--
deleteFunctionResponse
    :: DeleteFunctionResponse
deleteFunctionResponse = DeleteFunctionResponse'
