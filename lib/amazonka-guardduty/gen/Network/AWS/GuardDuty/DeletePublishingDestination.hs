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
-- Module      : Network.AWS.GuardDuty.DeletePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the publishing definition with the specified @destinationId@ .
module Network.AWS.GuardDuty.DeletePublishingDestination
  ( -- * Creating a Request
    deletePublishingDestination,
    DeletePublishingDestination,

    -- * Request Lenses
    dpdDetectorId,
    dpdDestinationId,

    -- * Destructuring the Response
    deletePublishingDestinationResponse,
    DeletePublishingDestinationResponse,

    -- * Response Lenses
    dpdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePublishingDestination' smart constructor.
data DeletePublishingDestination = DeletePublishingDestination'
  { _dpdDetectorId ::
      !Text,
    _dpdDestinationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePublishingDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpdDetectorId' - The unique ID of the detector associated with the publishing destination to delete.
--
-- * 'dpdDestinationId' - The ID of the publishing destination to delete.
deletePublishingDestination ::
  -- | 'dpdDetectorId'
  Text ->
  -- | 'dpdDestinationId'
  Text ->
  DeletePublishingDestination
deletePublishingDestination pDetectorId_ pDestinationId_ =
  DeletePublishingDestination'
    { _dpdDetectorId = pDetectorId_,
      _dpdDestinationId = pDestinationId_
    }

-- | The unique ID of the detector associated with the publishing destination to delete.
dpdDetectorId :: Lens' DeletePublishingDestination Text
dpdDetectorId = lens _dpdDetectorId (\s a -> s {_dpdDetectorId = a})

-- | The ID of the publishing destination to delete.
dpdDestinationId :: Lens' DeletePublishingDestination Text
dpdDestinationId = lens _dpdDestinationId (\s a -> s {_dpdDestinationId = a})

instance AWSRequest DeletePublishingDestination where
  type
    Rs DeletePublishingDestination =
      DeletePublishingDestinationResponse
  request = delete guardDuty
  response =
    receiveEmpty
      ( \s h x ->
          DeletePublishingDestinationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeletePublishingDestination

instance NFData DeletePublishingDestination

instance ToHeaders DeletePublishingDestination where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeletePublishingDestination where
  toPath DeletePublishingDestination' {..} =
    mconcat
      [ "/detector/",
        toBS _dpdDetectorId,
        "/publishingDestination/",
        toBS _dpdDestinationId
      ]

instance ToQuery DeletePublishingDestination where
  toQuery = const mempty

-- | /See:/ 'deletePublishingDestinationResponse' smart constructor.
newtype DeletePublishingDestinationResponse = DeletePublishingDestinationResponse'
  { _dpdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpdrsResponseStatus' - -- | The response status code.
deletePublishingDestinationResponse ::
  -- | 'dpdrsResponseStatus'
  Int ->
  DeletePublishingDestinationResponse
deletePublishingDestinationResponse pResponseStatus_ =
  DeletePublishingDestinationResponse'
    { _dpdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dpdrsResponseStatus :: Lens' DeletePublishingDestinationResponse Int
dpdrsResponseStatus = lens _dpdrsResponseStatus (\s a -> s {_dpdrsResponseStatus = a})

instance NFData DeletePublishingDestinationResponse
