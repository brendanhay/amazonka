{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.AddAttachmentsToSet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more attachments to an attachment set. If an AttachmentSetId is
-- not specified, a new attachment set is created, and the ID of the set is
-- returned in the response. If an AttachmentSetId is specified, the
-- attachments are added to the specified set, if it exists. An attachment set
-- is a temporary container for attachments that are to be added to a case or
-- case communication. The set is available for one hour after it is created;
-- the ExpiryTime returned in the response indicates when the set expires. The
-- maximum number of attachments in a set is 3, and the maximum size of any
-- attachment in the set is 5 MB.
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
    , aatsrAttachmentSetId
    , aatsrExpiryTime
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data AddAttachmentsToSet = AddAttachmentsToSet
    { _aatsAttachmentSetId :: Maybe Text
    , _aatsAttachments :: [Attachment]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddAttachmentsToSet' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentSetId ::@ @Maybe Text@
--
-- * @Attachments ::@ @[Attachment]@
--
addAttachmentsToSet :: [Attachment] -- ^ 'aatsAttachments'
                    -> AddAttachmentsToSet
addAttachmentsToSet p2 = AddAttachmentsToSet
    { _aatsAttachmentSetId = Nothing
    , _aatsAttachments = p2
    }

-- | The ID of the attachment set. If an AttachmentSetId is not specified, a new
-- attachment set is created, and the ID of the set is returned in the
-- response. If an AttachmentSetId is specified, the attachments are added to
-- the specified set, if it exists.
aatsAttachmentSetId :: Lens' AddAttachmentsToSet (Maybe Text)
aatsAttachmentSetId =
    lens _aatsAttachmentSetId (\s a -> s { _aatsAttachmentSetId = a })

-- | One or more attachments to add to the set. The limit is 3 attachments per
-- set, and the size limit is 5 MB per attachment.
aatsAttachments :: Lens' AddAttachmentsToSet [Attachment]
aatsAttachments = lens _aatsAttachments (\s a -> s { _aatsAttachments = a })

instance ToPath AddAttachmentsToSet

instance ToQuery AddAttachmentsToSet

instance ToHeaders AddAttachmentsToSet

instance ToJSON AddAttachmentsToSet

-- | The ID and expiry time of the attachment set returned by the
-- AddAttachmentsToSet operation.
data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse
    { _aatsrAttachmentSetId :: Maybe Text
    , _aatsrExpiryTime :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddAttachmentsToSetResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttachmentSetId ::@ @Maybe Text@
--
-- * @ExpiryTime ::@ @Maybe Text@
--
addAttachmentsToSetResponse :: AddAttachmentsToSetResponse
addAttachmentsToSetResponse = AddAttachmentsToSetResponse
    { _aatsrAttachmentSetId = Nothing
    , _aatsrExpiryTime = Nothing
    }

-- | The ID of the attachment set. If an AttachmentSetId was not specified, a
-- new attachment set is created, and the ID of the set is returned in the
-- response. If an AttachmentSetId was specified, the attachments are added to
-- the specified set, if it exists.
aatsrAttachmentSetId :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrAttachmentSetId =
    lens _aatsrAttachmentSetId (\s a -> s { _aatsrAttachmentSetId = a })

-- | The time and date when the attachment set expires.
aatsrExpiryTime :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrExpiryTime = lens _aatsrExpiryTime (\s a -> s { _aatsrExpiryTime = a })

instance FromJSON AddAttachmentsToSetResponse

instance AWSRequest AddAttachmentsToSet where
    type Sv AddAttachmentsToSet = Support
    type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse

    request = get
    response _ = jsonResponse
