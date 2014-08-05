{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.V2013_04_15.AddAttachmentsToSet
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
module Network.AWS.Support.V2013_04_15.AddAttachmentsToSet where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddAttachmentsToSet' request.
addAttachmentsToSet :: [Attachment] -- ^ '_aatsrAttachments'
                    -> AddAttachmentsToSet
addAttachmentsToSet p1 = AddAttachmentsToSet
    { _aatsrAttachments = p1
    , _aatsrAttachmentSetId = Nothing
    }

data AddAttachmentsToSet = AddAttachmentsToSet
    { _aatsrAttachments :: [Attachment]
      -- ^ One or more attachments to add to the set. The limit is 3
      -- attachments per set, and the size limit is 5 MB per attachment.
    , _aatsrAttachmentSetId :: Maybe Text
      -- ^ The ID of the attachment set. If an AttachmentSetId is not
      -- specified, a new attachment set is created, and the ID of the set
      -- is returned in the response. If an AttachmentSetId is specified,
      -- the attachments are added to the specified set, if it exists.
    } deriving (Show, Generic)

makeLenses ''AddAttachmentsToSet

instance ToPath AddAttachmentsToSet

instance ToQuery AddAttachmentsToSet

instance ToHeaders AddAttachmentsToSet

instance ToJSON AddAttachmentsToSet

data AddAttachmentsToSetResponse = AddAttachmentsToSetResponse
    { _aatssAttachmentSetId :: Maybe Text
      -- ^ The ID of the attachment set. If an AttachmentSetId was not
      -- specified, a new attachment set is created, and the ID of the set
      -- is returned in the response. If an AttachmentSetId was specified,
      -- the attachments are added to the specified set, if it exists.
    , _aatssExpiryTime :: Maybe Text
      -- ^ The time and date when the attachment set expires.
    } deriving (Show, Generic)

makeLenses ''AddAttachmentsToSetResponse

instance FromJSON AddAttachmentsToSetResponse

instance AWSRequest AddAttachmentsToSet where
    type Sv AddAttachmentsToSet = Support
    type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse

    request = get
    response _ = undefined
