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
-- Module      : Network.AWS.Athena.DeleteDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data catalog.
module Network.AWS.Athena.DeleteDataCatalog
  ( -- * Creating a Request
    deleteDataCatalog,
    DeleteDataCatalog,

    -- * Request Lenses
    ddcName,

    -- * Destructuring the Response
    deleteDataCatalogResponse,
    DeleteDataCatalogResponse,

    -- * Response Lenses
    ddcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDataCatalog' smart constructor.
newtype DeleteDataCatalog = DeleteDataCatalog' {_ddcName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDataCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcName' - The name of the data catalog to delete.
deleteDataCatalog ::
  -- | 'ddcName'
  Text ->
  DeleteDataCatalog
deleteDataCatalog pName_ = DeleteDataCatalog' {_ddcName = pName_}

-- | The name of the data catalog to delete.
ddcName :: Lens' DeleteDataCatalog Text
ddcName = lens _ddcName (\s a -> s {_ddcName = a})

instance AWSRequest DeleteDataCatalog where
  type Rs DeleteDataCatalog = DeleteDataCatalogResponse
  request = postJSON athena
  response =
    receiveEmpty
      (\s h x -> DeleteDataCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDataCatalog

instance NFData DeleteDataCatalog

instance ToHeaders DeleteDataCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonAthena.DeleteDataCatalog" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDataCatalog where
  toJSON DeleteDataCatalog' {..} =
    object (catMaybes [Just ("Name" .= _ddcName)])

instance ToPath DeleteDataCatalog where
  toPath = const "/"

instance ToQuery DeleteDataCatalog where
  toQuery = const mempty

-- | /See:/ 'deleteDataCatalogResponse' smart constructor.
newtype DeleteDataCatalogResponse = DeleteDataCatalogResponse'
  { _ddcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDataCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsResponseStatus' - -- | The response status code.
deleteDataCatalogResponse ::
  -- | 'ddcrsResponseStatus'
  Int ->
  DeleteDataCatalogResponse
deleteDataCatalogResponse pResponseStatus_ =
  DeleteDataCatalogResponse'
    { _ddcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddcrsResponseStatus :: Lens' DeleteDataCatalogResponse Int
ddcrsResponseStatus = lens _ddcrsResponseStatus (\s a -> s {_ddcrsResponseStatus = a})

instance NFData DeleteDataCatalogResponse
