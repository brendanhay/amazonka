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
-- Module      : Network.AWS.IoT.DeleteDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified dimension from your AWS account.
module Network.AWS.IoT.DeleteDimension
  ( -- * Creating a Request
    deleteDimension,
    DeleteDimension,

    -- * Request Lenses
    dName,

    -- * Destructuring the Response
    deleteDimensionResponse,
    DeleteDimensionResponse,

    -- * Response Lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDimension' smart constructor.
newtype DeleteDimension = DeleteDimension' {_dName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The unique identifier for the dimension that you want to delete.
deleteDimension ::
  -- | 'dName'
  Text ->
  DeleteDimension
deleteDimension pName_ = DeleteDimension' {_dName = pName_}

-- | The unique identifier for the dimension that you want to delete.
dName :: Lens' DeleteDimension Text
dName = lens _dName (\s a -> s {_dName = a})

instance AWSRequest DeleteDimension where
  type Rs DeleteDimension = DeleteDimensionResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DeleteDimensionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDimension

instance NFData DeleteDimension

instance ToHeaders DeleteDimension where
  toHeaders = const mempty

instance ToPath DeleteDimension where
  toPath DeleteDimension' {..} = mconcat ["/dimensions/", toBS _dName]

instance ToQuery DeleteDimension where
  toQuery = const mempty

-- | /See:/ 'deleteDimensionResponse' smart constructor.
newtype DeleteDimensionResponse = DeleteDimensionResponse'
  { _ddrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDimensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDimensionResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DeleteDimensionResponse
deleteDimensionResponse pResponseStatus_ =
  DeleteDimensionResponse' {_ddrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDimensionResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DeleteDimensionResponse
