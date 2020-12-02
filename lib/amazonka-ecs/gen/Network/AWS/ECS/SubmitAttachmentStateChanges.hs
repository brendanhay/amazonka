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
-- Module      : Network.AWS.ECS.SubmitAttachmentStateChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that an attachment changed states.
module Network.AWS.ECS.SubmitAttachmentStateChanges
  ( -- * Creating a Request
    submitAttachmentStateChanges,
    SubmitAttachmentStateChanges,

    -- * Request Lenses
    sascCluster,
    sascAttachments,

    -- * Destructuring the Response
    submitAttachmentStateChangesResponse,
    SubmitAttachmentStateChangesResponse,

    -- * Response Lenses
    sascrsAcknowledgment,
    sascrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'submitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { _sascCluster ::
      !(Maybe Text),
    _sascAttachments ::
      ![AttachmentStateChange]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubmitAttachmentStateChanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sascCluster' - The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
--
-- * 'sascAttachments' - Any attachments associated with the state change request.
submitAttachmentStateChanges ::
  SubmitAttachmentStateChanges
submitAttachmentStateChanges =
  SubmitAttachmentStateChanges'
    { _sascCluster = Nothing,
      _sascAttachments = mempty
    }

-- | The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
sascCluster :: Lens' SubmitAttachmentStateChanges (Maybe Text)
sascCluster = lens _sascCluster (\s a -> s {_sascCluster = a})

-- | Any attachments associated with the state change request.
sascAttachments :: Lens' SubmitAttachmentStateChanges [AttachmentStateChange]
sascAttachments = lens _sascAttachments (\s a -> s {_sascAttachments = a}) . _Coerce

instance AWSRequest SubmitAttachmentStateChanges where
  type
    Rs SubmitAttachmentStateChanges =
      SubmitAttachmentStateChangesResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          SubmitAttachmentStateChangesResponse'
            <$> (x .?> "acknowledgment") <*> (pure (fromEnum s))
      )

instance Hashable SubmitAttachmentStateChanges

instance NFData SubmitAttachmentStateChanges

instance ToHeaders SubmitAttachmentStateChanges where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SubmitAttachmentStateChanges where
  toJSON SubmitAttachmentStateChanges' {..} =
    object
      ( catMaybes
          [ ("cluster" .=) <$> _sascCluster,
            Just ("attachments" .= _sascAttachments)
          ]
      )

instance ToPath SubmitAttachmentStateChanges where
  toPath = const "/"

instance ToQuery SubmitAttachmentStateChanges where
  toQuery = const mempty

-- | /See:/ 'submitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { _sascrsAcknowledgment ::
      !(Maybe Text),
    _sascrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubmitAttachmentStateChangesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sascrsAcknowledgment' - Acknowledgement of the state change.
--
-- * 'sascrsResponseStatus' - -- | The response status code.
submitAttachmentStateChangesResponse ::
  -- | 'sascrsResponseStatus'
  Int ->
  SubmitAttachmentStateChangesResponse
submitAttachmentStateChangesResponse pResponseStatus_ =
  SubmitAttachmentStateChangesResponse'
    { _sascrsAcknowledgment =
        Nothing,
      _sascrsResponseStatus = pResponseStatus_
    }

-- | Acknowledgement of the state change.
sascrsAcknowledgment :: Lens' SubmitAttachmentStateChangesResponse (Maybe Text)
sascrsAcknowledgment = lens _sascrsAcknowledgment (\s a -> s {_sascrsAcknowledgment = a})

-- | -- | The response status code.
sascrsResponseStatus :: Lens' SubmitAttachmentStateChangesResponse Int
sascrsResponseStatus = lens _sascrsResponseStatus (\s a -> s {_sascrsResponseStatus = a})

instance NFData SubmitAttachmentStateChangesResponse
