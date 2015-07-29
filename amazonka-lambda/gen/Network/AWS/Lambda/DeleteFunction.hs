{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Lambda function code and configuration.
--
-- When you delete a function the associated access policy is also deleted.
-- You will need to delete the event source mappings explicitly.
--
-- This operation requires permission for the @lambda:DeleteFunction@
-- action.
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

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFunction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfFunctionName'
newtype DeleteFunction = DeleteFunction'
    { _dfFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFunction' smart constructor.
deleteFunction :: Text -> DeleteFunction
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
          = ["2015-03-31", "functions", toBS _dfFunctionName]

instance ToQuery DeleteFunction where
        toQuery = const mempty

-- | /See:/ 'deleteFunctionResponse' smart constructor.
data DeleteFunctionResponse =
    DeleteFunctionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFunctionResponse' smart constructor.
deleteFunctionResponse :: DeleteFunctionResponse
deleteFunctionResponse = DeleteFunctionResponse'
