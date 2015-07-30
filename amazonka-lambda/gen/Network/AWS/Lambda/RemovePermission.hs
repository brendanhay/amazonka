{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemovePermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- You can remove individual permissions from an access policy associated
-- with a Lambda function by providing a Statement ID.
--
-- Note that removal of a permission will cause an active event source to
-- lose permission to the function.
--
-- You need permission for the @lambda:RemovePermission@ action.
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

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removePermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpFunctionName'
--
-- * 'rpStatementId'
data RemovePermission = RemovePermission'
    { _rpFunctionName :: !Text
    , _rpStatementId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermission' smart constructor.
removePermission :: Text -> Text -> RemovePermission
removePermission pFunctionName_ pStatementId_ =
    RemovePermission'
    { _rpFunctionName = pFunctionName_
    , _rpStatementId = pStatementId_
    }

-- | Lambda function whose access policy you want to remove a permission
-- from.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
rpFunctionName :: Lens' RemovePermission Text
rpFunctionName = lens _rpFunctionName (\ s a -> s{_rpFunctionName = a});

-- | Statement ID of the permission to remove.
rpStatementId :: Lens' RemovePermission Text
rpStatementId = lens _rpStatementId (\ s a -> s{_rpStatementId = a});

instance AWSRequest RemovePermission where
        type Sv RemovePermission = Lambda
        type Rs RemovePermission = RemovePermissionResponse
        request = delete
        response = receiveNull RemovePermissionResponse'

instance ToHeaders RemovePermission where
        toHeaders = const mempty

instance ToPath RemovePermission where
        toPath RemovePermission'{..}
          = ["2015-03-31", "functions", toBS _rpFunctionName,
             "versions", "HEAD", "policy", toBS _rpStatementId]

instance ToQuery RemovePermission where
        toQuery = const mempty

-- | /See:/ 'removePermissionResponse' smart constructor.
data RemovePermissionResponse =
    RemovePermissionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermissionResponse' smart constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse'
