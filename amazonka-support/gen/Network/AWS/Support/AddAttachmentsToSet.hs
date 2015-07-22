{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more attachments to an attachment set. If an
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
    , aatsrqAttachmentSetId
    , aatsrqAttachments

    -- * Response
    , AddAttachmentsToSetResponse
    -- ** Response constructor
    , addAttachmentsToSetResponse
    -- ** Response lenses
    , aatsrsExpiryTime
    , aatsrsAttachmentSetId
    , aatsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'addAttachmentsToSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aatsrqAttachmentSetId'
--
-- * 'aatsrqAttachments'
data AddAttachmentsToSet = AddAttachmentsToSet'
    { _aatsrqAttachmentSetId :: !(Maybe Text)
    , _aatsrqAttachments     :: ![Attachment]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddAttachmentsToSet' smart constructor.
addAttachmentsToSet :: AddAttachmentsToSet
addAttachmentsToSet =
    AddAttachmentsToSet'
    { _aatsrqAttachmentSetId = Nothing
    , _aatsrqAttachments = mempty
    }

-- | The ID of the attachment set. If an @AttachmentSetId@ is not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @AttachmentSetId@ is specified, the attachments are
-- added to the specified set, if it exists.
aatsrqAttachmentSetId :: Lens' AddAttachmentsToSet (Maybe Text)
aatsrqAttachmentSetId = lens _aatsrqAttachmentSetId (\ s a -> s{_aatsrqAttachmentSetId = a});

-- | One or more attachments to add to the set. The limit is 3 attachments
-- per set, and the size limit is 5 MB per attachment.
aatsrqAttachments :: Lens' AddAttachmentsToSet [Attachment]
aatsrqAttachments = lens _aatsrqAttachments (\ s a -> s{_aatsrqAttachments = a});

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
                     <*> (pure (fromEnum s)))

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
              ["attachmentSetId" .= _aatsrqAttachmentSetId,
               "attachments" .= _aatsrqAttachments]

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
-- * 'aatsrsExpiryTime'
--
-- * 'aatsrsAttachmentSetId'
--
-- * 'aatsrsStatus'
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse'
    { _aatsrsExpiryTime      :: !(Maybe Text)
    , _aatsrsAttachmentSetId :: !(Maybe Text)
    , _aatsrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddAttachmentsToSetResponse' smart constructor.
addAttachmentsToSetResponse :: Int -> AddAttachmentsToSetResponse
addAttachmentsToSetResponse pStatus =
    AddAttachmentsToSetResponse'
    { _aatsrsExpiryTime = Nothing
    , _aatsrsAttachmentSetId = Nothing
    , _aatsrsStatus = pStatus
    }

-- | The time and date when the attachment set expires.
aatsrsExpiryTime :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrsExpiryTime = lens _aatsrsExpiryTime (\ s a -> s{_aatsrsExpiryTime = a});

-- | The ID of the attachment set. If an @AttachmentSetId@ was not specified,
-- a new attachment set is created, and the ID of the set is returned in
-- the response. If an @AttachmentSetId@ was specified, the attachments are
-- added to the specified set, if it exists.
aatsrsAttachmentSetId :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrsAttachmentSetId = lens _aatsrsAttachmentSetId (\ s a -> s{_aatsrsAttachmentSetId = a});

-- | FIXME: Undocumented member.
aatsrsStatus :: Lens' AddAttachmentsToSetResponse Int
aatsrsStatus = lens _aatsrsStatus (\ s a -> s{_aatsrsStatus = a});
