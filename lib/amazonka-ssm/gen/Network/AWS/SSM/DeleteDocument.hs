{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
module Network.AWS.SSM.DeleteDocument
  ( -- * Creating a Request
    deleteDocument,
    DeleteDocument,

    -- * Request Lenses
    dddVersionName,
    dddForce,
    dddDocumentVersion,
    dddName,

    -- * Destructuring the Response
    deleteDocumentResponse,
    DeleteDocumentResponse,

    -- * Response Lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'deleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { _dddVersionName ::
      !(Maybe Text),
    _dddForce :: !(Maybe Bool),
    _dddDocumentVersion :: !(Maybe Text),
    _dddName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddVersionName' - The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- * 'dddForce' - Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
--
-- * 'dddDocumentVersion' - The version of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- * 'dddName' - The name of the document.
deleteDocument ::
  -- | 'dddName'
  Text ->
  DeleteDocument
deleteDocument pName_ =
  DeleteDocument'
    { _dddVersionName = Nothing,
      _dddForce = Nothing,
      _dddDocumentVersion = Nothing,
      _dddName = pName_
    }

-- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
dddVersionName :: Lens' DeleteDocument (Maybe Text)
dddVersionName = lens _dddVersionName (\s a -> s {_dddVersionName = a})

-- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
dddForce :: Lens' DeleteDocument (Maybe Bool)
dddForce = lens _dddForce (\s a -> s {_dddForce = a})

-- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
dddDocumentVersion :: Lens' DeleteDocument (Maybe Text)
dddDocumentVersion = lens _dddDocumentVersion (\s a -> s {_dddDocumentVersion = a})

-- | The name of the document.
dddName :: Lens' DeleteDocument Text
dddName = lens _dddName (\s a -> s {_dddName = a})

instance AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request = postJSON ssm
  response =
    receiveEmpty
      (\s h x -> DeleteDocumentResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDocument

instance NFData DeleteDocument

instance ToHeaders DeleteDocument where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.DeleteDocument" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDocument where
  toJSON DeleteDocument' {..} =
    object
      ( catMaybes
          [ ("VersionName" .=) <$> _dddVersionName,
            ("Force" .=) <$> _dddForce,
            ("DocumentVersion" .=) <$> _dddDocumentVersion,
            Just ("Name" .= _dddName)
          ]
      )

instance ToPath DeleteDocument where
  toPath = const "/"

instance ToQuery DeleteDocument where
  toQuery = const mempty

-- | /See:/ 'deleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'
  { _ddrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDocumentResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DeleteDocumentResponse
deleteDocumentResponse pResponseStatus_ =
  DeleteDocumentResponse' {_ddrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDocumentResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DeleteDocumentResponse
