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
-- Module      : Network.AWS.SSM.DescribeDocumentPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (/All/ ).
--
--
module Network.AWS.SSM.DescribeDocumentPermission
    (
    -- * Creating a Request
      describeDocumentPermission
    , DescribeDocumentPermission
    -- * Request Lenses
    , ddpName
    , ddpPermissionType

    -- * Destructuring the Response
    , describeDocumentPermissionResponse
    , DescribeDocumentPermissionResponse
    -- * Response Lenses
    , ddprsAccountIds
    , ddprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeDocumentPermission' smart constructor.
data DescribeDocumentPermission = DescribeDocumentPermission'
  { _ddpName           :: !Text
  , _ddpPermissionType :: !DocumentPermissionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocumentPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpName' - The name of the document for which you are the owner.
--
-- * 'ddpPermissionType' - The permission type for the document. The permission type can be /Share/ .
describeDocumentPermission
    :: Text -- ^ 'ddpName'
    -> DocumentPermissionType -- ^ 'ddpPermissionType'
    -> DescribeDocumentPermission
describeDocumentPermission pName_ pPermissionType_ =
  DescribeDocumentPermission'
    {_ddpName = pName_, _ddpPermissionType = pPermissionType_}


-- | The name of the document for which you are the owner.
ddpName :: Lens' DescribeDocumentPermission Text
ddpName = lens _ddpName (\ s a -> s{_ddpName = a})

-- | The permission type for the document. The permission type can be /Share/ .
ddpPermissionType :: Lens' DescribeDocumentPermission DocumentPermissionType
ddpPermissionType = lens _ddpPermissionType (\ s a -> s{_ddpPermissionType = a})

instance AWSRequest DescribeDocumentPermission where
        type Rs DescribeDocumentPermission =
             DescribeDocumentPermissionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentPermissionResponse' <$>
                   (x .?> "AccountIds" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDocumentPermission where

instance NFData DescribeDocumentPermission where

instance ToHeaders DescribeDocumentPermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeDocumentPermission" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDocumentPermission where
        toJSON DescribeDocumentPermission'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _ddpName),
                  Just ("PermissionType" .= _ddpPermissionType)])

instance ToPath DescribeDocumentPermission where
        toPath = const "/"

instance ToQuery DescribeDocumentPermission where
        toQuery = const mempty

-- | /See:/ 'describeDocumentPermissionResponse' smart constructor.
data DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse'
  { _ddprsAccountIds     :: !(Maybe [Text])
  , _ddprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocumentPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddprsAccountIds' - The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
--
-- * 'ddprsResponseStatus' - -- | The response status code.
describeDocumentPermissionResponse
    :: Int -- ^ 'ddprsResponseStatus'
    -> DescribeDocumentPermissionResponse
describeDocumentPermissionResponse pResponseStatus_ =
  DescribeDocumentPermissionResponse'
    {_ddprsAccountIds = Nothing, _ddprsResponseStatus = pResponseStatus_}


-- | The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
ddprsAccountIds :: Lens' DescribeDocumentPermissionResponse [Text]
ddprsAccountIds = lens _ddprsAccountIds (\ s a -> s{_ddprsAccountIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ddprsResponseStatus :: Lens' DescribeDocumentPermissionResponse Int
ddprsResponseStatus = lens _ddprsResponseStatus (\ s a -> s{_ddprsResponseStatus = a})

instance NFData DescribeDocumentPermissionResponse
         where
