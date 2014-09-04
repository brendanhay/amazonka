{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.AddCommunicationToCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds additional customer communication to an AWS Support case. You use the
-- CaseId value to identify the case to add communication to. You can list a
-- set of email addresses to copy on the communication using the
-- CcEmailAddresses value. The CommunicationBody value contains the text of
-- the communication. The response indicates the success or failure of the
-- request. This operation implements a subset of the behavior on the AWS
-- Support Your Support Cases web form.
module Network.AWS.Support.V2013_04_15.AddCommunicationToCase
    (
    -- * Request
      AddCommunicationToCase
    -- ** Request constructor
    , addCommunicationToCase
    -- ** Request lenses
    , actcrCommunicationBody
    , actcrAttachmentSetId
    , actcrCaseId
    , actcrCcEmailAddresses

    -- * Response
    , AddCommunicationToCaseResponse
    -- ** Response lenses
    , actcsResult
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AddCommunicationToCase' request.
addCommunicationToCase :: Text -- ^ 'actcrCommunicationBody'
                       -> AddCommunicationToCase
addCommunicationToCase p1 = AddCommunicationToCase
    { _actcrCommunicationBody = p1
    , _actcrAttachmentSetId = Nothing
    , _actcrCaseId = Nothing
    , _actcrCcEmailAddresses = mempty
    }
{-# INLINE addCommunicationToCase #-}

data AddCommunicationToCase = AddCommunicationToCase
    { _actcrCommunicationBody :: Text
      -- ^ The body of an email communication to add to the support case.
    , _actcrAttachmentSetId :: Maybe Text
      -- ^ The ID of a set of one or more attachments for the communication
      -- to add to the case. Create the set by calling
      -- AddAttachmentsToSet.
    , _actcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    , _actcrCcEmailAddresses :: [Text]
      -- ^ The email addresses in the CC line of an email to be added to the
      -- support case.
    } deriving (Show, Generic)

-- | The body of an email communication to add to the support case.
actcrCommunicationBody :: Lens' AddCommunicationToCase (Text)
actcrCommunicationBody f x =
    f (_actcrCommunicationBody x)
        <&> \y -> x { _actcrCommunicationBody = y }
{-# INLINE actcrCommunicationBody #-}

-- | The ID of a set of one or more attachments for the communication to add to
-- the case. Create the set by calling AddAttachmentsToSet.
actcrAttachmentSetId :: Lens' AddCommunicationToCase (Maybe Text)
actcrAttachmentSetId f x =
    f (_actcrAttachmentSetId x)
        <&> \y -> x { _actcrAttachmentSetId = y }
{-# INLINE actcrAttachmentSetId #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
actcrCaseId :: Lens' AddCommunicationToCase (Maybe Text)
actcrCaseId f x =
    f (_actcrCaseId x)
        <&> \y -> x { _actcrCaseId = y }
{-# INLINE actcrCaseId #-}

-- | The email addresses in the CC line of an email to be added to the support
-- case.
actcrCcEmailAddresses :: Lens' AddCommunicationToCase ([Text])
actcrCcEmailAddresses f x =
    f (_actcrCcEmailAddresses x)
        <&> \y -> x { _actcrCcEmailAddresses = y }
{-# INLINE actcrCcEmailAddresses #-}

instance ToPath AddCommunicationToCase

instance ToQuery AddCommunicationToCase

instance ToHeaders AddCommunicationToCase

instance ToJSON AddCommunicationToCase

data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcsResult :: Maybe Bool
      -- ^ True if AddCommunicationToCase succeeds. Otherwise, returns an
      -- error.
    } deriving (Show, Generic)

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
actcsResult :: Lens' AddCommunicationToCaseResponse (Maybe Bool)
actcsResult f x =
    f (_actcsResult x)
        <&> \y -> x { _actcsResult = y }
{-# INLINE actcsResult #-}

instance FromJSON AddCommunicationToCaseResponse

instance AWSRequest AddCommunicationToCase where
    type Sv AddCommunicationToCase = Support
    type Rs AddCommunicationToCase = AddCommunicationToCaseResponse

    request = get
    response _ = jsonResponse
