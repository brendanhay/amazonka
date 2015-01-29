{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

-- | Adds one or more attachments to an attachment set. If an 'AttachmentSetId' is
-- not specified, a new attachment set is created, and the ID of the set is
-- returned in the response. If an 'AttachmentSetId' is specified, the attachments
-- are added to the specified set, if it exists.
--
-- An attachment set is a temporary container for attachments that are to be
-- added to a case or case communication. The set is available for one hour
-- after it is created; the 'ExpiryTime' returned in the response indicates when
-- the set expires. The maximum number of attachments in a set is 3, and the
-- maximum size of any attachment in the set is 5 MB.
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
    , aatsrAttachmentSetId
    , aatsrExpiryTime
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

data AddAttachmentsToSet = AddAttachmentsToSet
    { _aatsAttachmentSetId :: Maybe Text
    , _aatsAttachments     :: List "attachments" Attachment
    } deriving (Eq, Read, Show)

-- | 'AddAttachmentsToSet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aatsAttachmentSetId' @::@ 'Maybe' 'Text'
--
-- * 'aatsAttachments' @::@ ['Attachment']
--
addAttachmentsToSet :: AddAttachmentsToSet
addAttachmentsToSet = AddAttachmentsToSet
    { _aatsAttachmentSetId = Nothing
    , _aatsAttachments     = mempty
    }

-- | The ID of the attachment set. If an 'AttachmentSetId' is not specified, a new
-- attachment set is created, and the ID of the set is returned in the response.
-- If an 'AttachmentSetId' is specified, the attachments are added to the
-- specified set, if it exists.
aatsAttachmentSetId :: Lens' AddAttachmentsToSet (Maybe Text)
aatsAttachmentSetId =
    lens _aatsAttachmentSetId (\s a -> s { _aatsAttachmentSetId = a })

-- | One or more attachments to add to the set. The limit is 3 attachments per
-- set, and the size limit is 5 MB per attachment.
aatsAttachments :: Lens' AddAttachmentsToSet [Attachment]
aatsAttachments = lens _aatsAttachments (\s a -> s { _aatsAttachments = a }) . _List

data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse
    { _aatsrAttachmentSetId :: Maybe Text
    , _aatsrExpiryTime      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AddAttachmentsToSetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aatsrAttachmentSetId' @::@ 'Maybe' 'Text'
--
-- * 'aatsrExpiryTime' @::@ 'Maybe' 'Text'
--
addAttachmentsToSetResponse :: AddAttachmentsToSetResponse
addAttachmentsToSetResponse = AddAttachmentsToSetResponse
    { _aatsrAttachmentSetId = Nothing
    , _aatsrExpiryTime      = Nothing
    }

-- | The ID of the attachment set. If an 'AttachmentSetId' was not specified, a new
-- attachment set is created, and the ID of the set is returned in the response.
-- If an 'AttachmentSetId' was specified, the attachments are added to the
-- specified set, if it exists.
aatsrAttachmentSetId :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrAttachmentSetId =
    lens _aatsrAttachmentSetId (\s a -> s { _aatsrAttachmentSetId = a })

-- | The time and date when the attachment set expires.
aatsrExpiryTime :: Lens' AddAttachmentsToSetResponse (Maybe Text)
aatsrExpiryTime = lens _aatsrExpiryTime (\s a -> s { _aatsrExpiryTime = a })

instance ToPath AddAttachmentsToSet where
    toPath = const "/"

instance ToQuery AddAttachmentsToSet where
    toQuery = const mempty

instance ToHeaders AddAttachmentsToSet

instance ToJSON AddAttachmentsToSet where
    toJSON AddAttachmentsToSet{..} = object
        [ "attachmentSetId" .= _aatsAttachmentSetId
        , "attachments"     .= _aatsAttachments
        ]

instance AWSRequest AddAttachmentsToSet where
    type Sv AddAttachmentsToSet = Support
    type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse

    request  = post "AddAttachmentsToSet"
    response = jsonResponse

instance FromJSON AddAttachmentsToSetResponse where
    parseJSON = withObject "AddAttachmentsToSetResponse" $ \o -> AddAttachmentsToSetResponse
        <$> o .:? "attachmentSetId"
        <*> o .:? "expiryTime"
