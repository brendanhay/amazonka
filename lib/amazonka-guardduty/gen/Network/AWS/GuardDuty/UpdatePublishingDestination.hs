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
-- Module      : Network.AWS.GuardDuty.UpdatePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the publishing destination specified by the @destinationId@ .
module Network.AWS.GuardDuty.UpdatePublishingDestination
  ( -- * Creating a Request
    updatePublishingDestination,
    UpdatePublishingDestination,

    -- * Request Lenses
    updDestinationProperties,
    updDetectorId,
    updDestinationId,

    -- * Destructuring the Response
    updatePublishingDestinationResponse,
    UpdatePublishingDestinationResponse,

    -- * Response Lenses
    updrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePublishingDestination' smart constructor.
data UpdatePublishingDestination = UpdatePublishingDestination'
  { _updDestinationProperties ::
      !(Maybe DestinationProperties),
    _updDetectorId :: !Text,
    _updDestinationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePublishingDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updDestinationProperties' - A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- * 'updDetectorId' - The ID of the detector associated with the publishing destinations to update.
--
-- * 'updDestinationId' - The ID of the publishing destination to update.
updatePublishingDestination ::
  -- | 'updDetectorId'
  Text ->
  -- | 'updDestinationId'
  Text ->
  UpdatePublishingDestination
updatePublishingDestination pDetectorId_ pDestinationId_ =
  UpdatePublishingDestination'
    { _updDestinationProperties = Nothing,
      _updDetectorId = pDetectorId_,
      _updDestinationId = pDestinationId_
    }

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
updDestinationProperties :: Lens' UpdatePublishingDestination (Maybe DestinationProperties)
updDestinationProperties = lens _updDestinationProperties (\s a -> s {_updDestinationProperties = a})

-- | The ID of the detector associated with the publishing destinations to update.
updDetectorId :: Lens' UpdatePublishingDestination Text
updDetectorId = lens _updDetectorId (\s a -> s {_updDetectorId = a})

-- | The ID of the publishing destination to update.
updDestinationId :: Lens' UpdatePublishingDestination Text
updDestinationId = lens _updDestinationId (\s a -> s {_updDestinationId = a})

instance AWSRequest UpdatePublishingDestination where
  type
    Rs UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request = postJSON guardDuty
  response =
    receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdatePublishingDestination

instance NFData UpdatePublishingDestination

instance ToHeaders UpdatePublishingDestination where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination' {..} =
    object
      ( catMaybes
          [("destinationProperties" .=) <$> _updDestinationProperties]
      )

instance ToPath UpdatePublishingDestination where
  toPath UpdatePublishingDestination' {..} =
    mconcat
      [ "/detector/",
        toBS _updDetectorId,
        "/publishingDestination/",
        toBS _updDestinationId
      ]

instance ToQuery UpdatePublishingDestination where
  toQuery = const mempty

-- | /See:/ 'updatePublishingDestinationResponse' smart constructor.
newtype UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { _updrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updrsResponseStatus' - -- | The response status code.
updatePublishingDestinationResponse ::
  -- | 'updrsResponseStatus'
  Int ->
  UpdatePublishingDestinationResponse
updatePublishingDestinationResponse pResponseStatus_ =
  UpdatePublishingDestinationResponse'
    { _updrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
updrsResponseStatus :: Lens' UpdatePublishingDestinationResponse Int
updrsResponseStatus = lens _updrsResponseStatus (\s a -> s {_updrsResponseStatus = a})

instance NFData UpdatePublishingDestinationResponse
