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
-- Module      : Network.AWS.EMR.DeleteStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an Amazon EMR Studio from the Studio metadata store.
module Network.AWS.EMR.DeleteStudio
  ( -- * Creating a Request
    deleteStudio,
    DeleteStudio,

    -- * Request Lenses
    dStudioId,

    -- * Destructuring the Response
    deleteStudioResponse,
    DeleteStudioResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStudio' smart constructor.
newtype DeleteStudio = DeleteStudio' {_dStudioId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStudio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStudioId' - The ID of the Amazon EMR Studio.
deleteStudio ::
  -- | 'dStudioId'
  Text ->
  DeleteStudio
deleteStudio pStudioId_ = DeleteStudio' {_dStudioId = pStudioId_}

-- | The ID of the Amazon EMR Studio.
dStudioId :: Lens' DeleteStudio Text
dStudioId = lens _dStudioId (\s a -> s {_dStudioId = a})

instance AWSRequest DeleteStudio where
  type Rs DeleteStudio = DeleteStudioResponse
  request = postJSON emr
  response = receiveNull DeleteStudioResponse'

instance Hashable DeleteStudio

instance NFData DeleteStudio

instance ToHeaders DeleteStudio where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ElasticMapReduce.DeleteStudio" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteStudio where
  toJSON DeleteStudio' {..} =
    object (catMaybes [Just ("StudioId" .= _dStudioId)])

instance ToPath DeleteStudio where
  toPath = const "/"

instance ToQuery DeleteStudio where
  toQuery = const mempty

-- | /See:/ 'deleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStudioResponse' with the minimum fields required to make a request.
deleteStudioResponse ::
  DeleteStudioResponse
deleteStudioResponse = DeleteStudioResponse'

instance NFData DeleteStudioResponse
