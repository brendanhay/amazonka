{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.Support.V2013_04_15.AddAttachmentsToSet
    (
    -- * Request
      AddAttachmentsToSet
    -- ** Request constructor
    , addAttachmentsToSet
    -- ** Request lenses
    , aatsrAttachments
    , aatsrAttachmentSetId

    -- * Response
    , AddAttachmentsToSetResponse
    -- ** Response lenses
    , aatssAttachmentSetId
    , aatssExpiryTime
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AddAttachmentsToSet' request.
addAttachmentsToSet :: [Attachment] -- ^ 'aatsrAttachments'
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

-- | One or more attachments to add to the set. The limit is 3 attachments per
-- set, and the size limit is 5 MB per attachment.
aatsrAttachments
    :: Functor f
    => ([Attachment]
    -> f ([Attachment]))
    -> AddAttachmentsToSet
    -> f AddAttachmentsToSet
aatsrAttachments f x =
    (\y -> x { _aatsrAttachments = y })
       <$> f (_aatsrAttachments x)
{-# INLINE aatsrAttachments #-}

-- | The ID of the attachment set. If an AttachmentSetId is not specified, a new
-- attachment set is created, and the ID of the set is returned in the
-- response. If an AttachmentSetId is specified, the attachments are added to
-- the specified set, if it exists.
aatsrAttachmentSetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddAttachmentsToSet
    -> f AddAttachmentsToSet
aatsrAttachmentSetId f x =
    (\y -> x { _aatsrAttachmentSetId = y })
       <$> f (_aatsrAttachmentSetId x)
{-# INLINE aatsrAttachmentSetId #-}

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

-- | The ID of the attachment set. If an AttachmentSetId was not specified, a
-- new attachment set is created, and the ID of the set is returned in the
-- response. If an AttachmentSetId was specified, the attachments are added to
-- the specified set, if it exists.
aatssAttachmentSetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddAttachmentsToSetResponse
    -> f AddAttachmentsToSetResponse
aatssAttachmentSetId f x =
    (\y -> x { _aatssAttachmentSetId = y })
       <$> f (_aatssAttachmentSetId x)
{-# INLINE aatssAttachmentSetId #-}

-- | The time and date when the attachment set expires.
aatssExpiryTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddAttachmentsToSetResponse
    -> f AddAttachmentsToSetResponse
aatssExpiryTime f x =
    (\y -> x { _aatssExpiryTime = y })
       <$> f (_aatssExpiryTime x)
{-# INLINE aatssExpiryTime #-}

instance FromJSON AddAttachmentsToSetResponse

instance AWSRequest AddAttachmentsToSet where
    type Sv AddAttachmentsToSet = Support
    type Rs AddAttachmentsToSet = AddAttachmentsToSetResponse

    request = get
    response _ = jsonResponse
