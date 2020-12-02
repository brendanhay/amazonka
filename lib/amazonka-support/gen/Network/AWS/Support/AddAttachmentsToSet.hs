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
-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more attachments to an attachment set.
--
--
-- An attachment set is a temporary container for attachments that you add to a case or case communication. The set is available for 1 hour after it's created. The @expiryTime@ returned in the response is when the set expires.
module Network.AWS.Support.AddAttachmentsToSet
  ( -- * Creating a Request
    addAttachmentsToSet,
    AddAttachmentsToSet,

    -- * Request Lenses
    aatsAttachmentSetId,
    aatsAttachments,

    -- * Destructuring the Response
    addAttachmentsToSetResponse,
    AddAttachmentsToSetResponse,

    -- * Response Lenses
    aatsrsExpiryTime,
    aatsrsAttachmentSetId,
    aatsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types

-- | /See:/ 'addAttachmentsToSet' smart constructor.
data AddAttachmentsToSet = AddAttachmentsToSet'
  { _aatsAttachmentSetId ::
      !(Maybe Text),
    _aatsAttachments :: ![Attachment]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddAttachmentsToSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatsAttachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
--
-- * 'aatsAttachments' - One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment. In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
addAttachmentsToSet ::
  AddAttachmentsToSet
addAttachmentsToSet =
  AddAttachmentsToSet'
    { _aatsAttachmentSetId = Nothing,
      _aatsAttachments = mempty
    }

-- | The ID of the attachment set. If an @attachmentSetId@ is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ is specified, the attachments are added to the specified set, if it exists.
aatsAttachmentSetId :: Lens' AddAttachmentsToSet (Maybe Text)
aatsAttachmentSetId = lens _aatsAttachmentSetId (\s a -> s {_aatsAttachmentSetId = a})

-- | One or more attachments to add to the set. You can add up to three attachments per set. The size limit is 5 MB per attachment. In the @Attachment@ object, use the @data@ parameter to specify the contents of the attachment file. In the previous request syntax, the value for @data@ appear as @blob@ , which is represented as a base64-encoded string. The value for @fileName@ is the name of the attachment, such as @troubleshoot-screenshot.png@ .
aatsAttachments :: Lens' AddAttachmentsToSet [Attachment]
aatsAttachments = lens _aatsAttachments (\s a -> s {_aatsAttachments = a}) . _Coerce

instance AWSRequest AddAttachmentsToSet where
  type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse
  request = postJSON support
  response =
    receiveJSON
      ( \s h x ->
          AddAttachmentsToSetResponse'
            <$> (x .?> "expiryTime")
            <*> (x .?> "attachmentSetId")
            <*> (pure (fromEnum s))
      )

instance Hashable AddAttachmentsToSet

instance NFData AddAttachmentsToSet

instance ToHeaders AddAttachmentsToSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSSupport_20130415.AddAttachmentsToSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AddAttachmentsToSet where
  toJSON AddAttachmentsToSet' {..} =
    object
      ( catMaybes
          [ ("attachmentSetId" .=) <$> _aatsAttachmentSetId,
            Just ("attachments" .= _aatsAttachments)
          ]
      )

instance ToPath AddAttachmentsToSet where
  toPath = const "/"

instance ToQuery AddAttachmentsToSet where
  toQuery = const mempty

-- | The ID and expiry time of the attachment set returned by the 'AddAttachmentsToSet' operation.
--
--
--
-- /See:/ 'addAttachmentsToSetResponse' smart constructor.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
  { _aatsrsExpiryTime ::
      !(Maybe Text),
    _aatsrsAttachmentSetId ::
      !(Maybe Text),
    _aatsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddAttachmentsToSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatsrsExpiryTime' - The time and date when the attachment set expires.
--
-- * 'aatsrsAttachmentSetId' - The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
--
-- * 'aatsrsResponseStatus' - -- | The response status code.
addAttachmentsToSetResponse ::
  -- | 'aatsrsResponseStatus'
  Int ->
  AddAttachmentsToSetResponse
addAttachmentsToSetResponse pResponseStatus_ =
  AddAttachmentsToSetResponse'
    { _aatsrsExpiryTime = Nothing,
      _aatsrsAttachmentSetId = Nothing,
      _aatsrsResponseStatus = pResponseStatus_
    }

-- | The time and date when the attachment set expires.
aatsrsExpiryTime :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrsExpiryTime = lens _aatsrsExpiryTime (\s a -> s {_aatsrsExpiryTime = a})

-- | The ID of the attachment set. If an @attachmentSetId@ was not specified, a new attachment set is created, and the ID of the set is returned in the response. If an @attachmentSetId@ was specified, the attachments are added to the specified set, if it exists.
aatsrsAttachmentSetId :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrsAttachmentSetId = lens _aatsrsAttachmentSetId (\s a -> s {_aatsrsAttachmentSetId = a})

-- | -- | The response status code.
aatsrsResponseStatus :: Lens' AddAttachmentsToSetResponse Int
aatsrsResponseStatus = lens _aatsrsResponseStatus (\s a -> s {_aatsrsResponseStatus = a})

instance NFData AddAttachmentsToSetResponse
