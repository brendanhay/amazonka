{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds one or more attachments to an attachment set. If an
-- @AttachmentSetId@ is not specified, a new attachment set is created, and
-- the ID of the set is returned in the response. If an @AttachmentSetId@
-- is specified, the attachments are added to the specified set, if it
-- exists.
--
-- An attachment set is a temporary container for attachments that are to
-- be added to a case or case communication. The set is available for one
-- hour after it is created; the @ExpiryTime@ returned in the response
-- indicates when the set expires. The maximum number of attachments in a
-- set is 3, and the maximum size of any attachment in the set is 5 MB.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_AddAttachmentsToSet.html>
module Network.AWS.Support.AddAttachmentsToSet
    (
    -- * Request
      AddAttachmentsToSet
    -- ** Request constructor
    , addAttachmentsToSet
    -- ** Request lenses
    , aatsAttachmentSetId
    , aatsAttachments

    -- * Response
    , AddAttachmentsToSetResponse
    -- ** Response constructor
    , addAttachmentsToSetResponse
    -- ** Response lenses
    , aatsrExpiryTime
    , aatsrAttachmentSetId
    , aatsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'addAttachmentsToSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aatsAttachmentSetId'
--
-- * 'aatsAttachments'
data AddAttachmentsToSet = AddAttachmentsToSet'
    { _aatsAttachmentSetId :: !(Maybe Text)
    , _aatsAttachments     :: ![Attachment]
    } deriving (Eq,Read,Show)

-- | 'AddAttachmentsToSet' smart constructor.
addAttachmentsToSet :: AddAttachmentsToSet
addAttachmentsToSet =
    AddAttachmentsToSet'
    { _aatsAttachmentSetId = Nothing
    , _aatsAttachments = mempty
    }

-- | The ID of the attachment set. If an @AttachmentSetId@ is not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @AttachmentSetId@ is specified, the attachments are
-- added to the specified set, if it exists.
aatsAttachmentSetId :: Lens' AddAttachmentsToSet (Maybe Text)
aatsAttachmentSetId = lens _aatsAttachmentSetId (\ s a -> s{_aatsAttachmentSetId = a});

-- | One or more attachments to add to the set. The limit is 3 attachments
-- per set, and the size limit is 5 MB per attachment.
aatsAttachments :: Lens' AddAttachmentsToSet [Attachment]
aatsAttachments = lens _aatsAttachments (\ s a -> s{_aatsAttachments = a});

instance AWSRequest AddAttachmentsToSet where
        type Sv AddAttachmentsToSet = Support
        type Rs AddAttachmentsToSet =
             AddAttachmentsToSetResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddAttachmentsToSetResponse' <$>
                   (x .?> "expiryTime") <*> (x .?> "attachmentSetId")
                     <*> (pure s))

instance ToHeaders AddAttachmentsToSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.AddAttachmentsToSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddAttachmentsToSet where
        toJSON AddAttachmentsToSet'{..}
          = object
              ["attachmentSetId" .= _aatsAttachmentSetId,
               "attachments" .= _aatsAttachments]

instance ToPath AddAttachmentsToSet where
        toPath = const "/"

instance ToQuery AddAttachmentsToSet where
        toQuery = const mempty

-- | The ID and expiry time of the attachment set returned by the
-- AddAttachmentsToSet operation.
--
-- /See:/ 'addAttachmentsToSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aatsrExpiryTime'
--
-- * 'aatsrAttachmentSetId'
--
-- * 'aatsrStatus'
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
    { _aatsrExpiryTime      :: !(Maybe Text)
    , _aatsrAttachmentSetId :: !(Maybe Text)
    , _aatsrStatus          :: !Status
    } deriving (Eq,Show)

-- | 'AddAttachmentsToSetResponse' smart constructor.
addAttachmentsToSetResponse :: Status -> AddAttachmentsToSetResponse
addAttachmentsToSetResponse pStatus =
    AddAttachmentsToSetResponse'
    { _aatsrExpiryTime = Nothing
    , _aatsrAttachmentSetId = Nothing
    , _aatsrStatus = pStatus
    }

-- | The time and date when the attachment set expires.
aatsrExpiryTime :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrExpiryTime = lens _aatsrExpiryTime (\ s a -> s{_aatsrExpiryTime = a});

-- | The ID of the attachment set. If an @AttachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @AttachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
aatsrAttachmentSetId :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrAttachmentSetId = lens _aatsrAttachmentSetId (\ s a -> s{_aatsrAttachmentSetId = a});

-- | FIXME: Undocumented member.
aatsrStatus :: Lens' AddAttachmentsToSetResponse Status
aatsrStatus = lens _aatsrStatus (\ s a -> s{_aatsrStatus = a});
