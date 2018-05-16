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
-- Module      : Network.AWS.SSM.ModifyDocumentPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify /All/ as the account ID.
--
--
module Network.AWS.SSM.ModifyDocumentPermission
    (
    -- * Creating a Request
      modifyDocumentPermission
    , ModifyDocumentPermission
    -- * Request Lenses
    , mdpAccountIdsToAdd
    , mdpAccountIdsToRemove
    , mdpName
    , mdpPermissionType

    -- * Destructuring the Response
    , modifyDocumentPermissionResponse
    , ModifyDocumentPermissionResponse
    -- * Response Lenses
    , mdprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'modifyDocumentPermission' smart constructor.
data ModifyDocumentPermission = ModifyDocumentPermission'
  { _mdpAccountIdsToAdd    :: !(Maybe [Text])
  , _mdpAccountIdsToRemove :: !(Maybe [Text])
  , _mdpName               :: !Text
  , _mdpPermissionType     :: !DocumentPermissionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDocumentPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdpAccountIdsToAdd' - The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
--
-- * 'mdpAccountIdsToRemove' - The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
--
-- * 'mdpName' - The name of the document that you want to share.
--
-- * 'mdpPermissionType' - The permission type for the document. The permission type can be /Share/ .
modifyDocumentPermission
    :: Text -- ^ 'mdpName'
    -> DocumentPermissionType -- ^ 'mdpPermissionType'
    -> ModifyDocumentPermission
modifyDocumentPermission pName_ pPermissionType_ =
  ModifyDocumentPermission'
    { _mdpAccountIdsToAdd = Nothing
    , _mdpAccountIdsToRemove = Nothing
    , _mdpName = pName_
    , _mdpPermissionType = pPermissionType_
    }


-- | The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
mdpAccountIdsToAdd :: Lens' ModifyDocumentPermission [Text]
mdpAccountIdsToAdd = lens _mdpAccountIdsToAdd (\ s a -> s{_mdpAccountIdsToAdd = a}) . _Default . _Coerce

-- | The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
mdpAccountIdsToRemove :: Lens' ModifyDocumentPermission [Text]
mdpAccountIdsToRemove = lens _mdpAccountIdsToRemove (\ s a -> s{_mdpAccountIdsToRemove = a}) . _Default . _Coerce

-- | The name of the document that you want to share.
mdpName :: Lens' ModifyDocumentPermission Text
mdpName = lens _mdpName (\ s a -> s{_mdpName = a})

-- | The permission type for the document. The permission type can be /Share/ .
mdpPermissionType :: Lens' ModifyDocumentPermission DocumentPermissionType
mdpPermissionType = lens _mdpPermissionType (\ s a -> s{_mdpPermissionType = a})

instance AWSRequest ModifyDocumentPermission where
        type Rs ModifyDocumentPermission =
             ModifyDocumentPermissionResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 ModifyDocumentPermissionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ModifyDocumentPermission where

instance NFData ModifyDocumentPermission where

instance ToHeaders ModifyDocumentPermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ModifyDocumentPermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyDocumentPermission where
        toJSON ModifyDocumentPermission'{..}
          = object
              (catMaybes
                 [("AccountIdsToAdd" .=) <$> _mdpAccountIdsToAdd,
                  ("AccountIdsToRemove" .=) <$> _mdpAccountIdsToRemove,
                  Just ("Name" .= _mdpName),
                  Just ("PermissionType" .= _mdpPermissionType)])

instance ToPath ModifyDocumentPermission where
        toPath = const "/"

instance ToQuery ModifyDocumentPermission where
        toQuery = const mempty

-- | /See:/ 'modifyDocumentPermissionResponse' smart constructor.
newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse'
  { _mdprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDocumentPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdprsResponseStatus' - -- | The response status code.
modifyDocumentPermissionResponse
    :: Int -- ^ 'mdprsResponseStatus'
    -> ModifyDocumentPermissionResponse
modifyDocumentPermissionResponse pResponseStatus_ =
  ModifyDocumentPermissionResponse' {_mdprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
mdprsResponseStatus :: Lens' ModifyDocumentPermissionResponse Int
mdprsResponseStatus = lens _mdprsResponseStatus (\ s a -> s{_mdprsResponseStatus = a})

instance NFData ModifyDocumentPermissionResponse
         where
