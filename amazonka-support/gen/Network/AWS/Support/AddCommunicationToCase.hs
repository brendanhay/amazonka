{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.AddCommunicationToCase
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
module Network.AWS.Support.AddCommunicationToCase
    (
    -- * Request
      AddCommunicationToCase
    -- ** Request constructor
    , addCommunicationToCase
    -- ** Request lenses
    , actcCaseId
    , actcCommunicationBody
    , actcCcEmailAddresses
    , actcAttachmentSetId

    -- * Response
    , AddCommunicationToCaseResponse
    -- ** Response constructor
    , addCommunicationToCaseResponse
    -- ** Response lenses
    , actcrResult
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | To be written.
data AddCommunicationToCase = AddCommunicationToCase
    { _actcCaseId :: Maybe Text
    , _actcCommunicationBody :: Text
    , _actcCcEmailAddresses :: [Text]
    , _actcAttachmentSetId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddCommunicationToCase' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CaseId ::@ @Maybe Text@
--
-- * @CommunicationBody ::@ @Text@
--
-- * @CcEmailAddresses ::@ @[Text]@
--
-- * @AttachmentSetId ::@ @Maybe Text@
--
addCommunicationToCase :: Text -- ^ 'actcCommunicationBody'
                       -> AddCommunicationToCase
addCommunicationToCase p2 = AddCommunicationToCase
    { _actcCaseId = Nothing
    , _actcCommunicationBody = p2
    , _actcCcEmailAddresses = mempty
    , _actcAttachmentSetId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
actcCaseId :: Lens' AddCommunicationToCase (Maybe Text)
actcCaseId = lens _actcCaseId (\s a -> s { _actcCaseId = a })

-- | The body of an email communication to add to the support case.
actcCommunicationBody :: Lens' AddCommunicationToCase Text
actcCommunicationBody =
    lens _actcCommunicationBody (\s a -> s { _actcCommunicationBody = a })

-- | The email addresses in the CC line of an email to be added to the support
-- case.
actcCcEmailAddresses :: Lens' AddCommunicationToCase [Text]
actcCcEmailAddresses =
    lens _actcCcEmailAddresses (\s a -> s { _actcCcEmailAddresses = a })

-- | The ID of a set of one or more attachments for the communication to add to
-- the case. Create the set by calling AddAttachmentsToSet.
actcAttachmentSetId :: Lens' AddCommunicationToCase (Maybe Text)
actcAttachmentSetId =
    lens _actcAttachmentSetId (\s a -> s { _actcAttachmentSetId = a })

instance ToPath AddCommunicationToCase

instance ToQuery AddCommunicationToCase

instance ToHeaders AddCommunicationToCase

instance ToJSON AddCommunicationToCase

-- | The result of the AddCommunicationToCase operation.
newtype AddCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcrResult :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddCommunicationToCaseResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Result ::@ @Maybe Bool@
--
addCommunicationToCaseResponse :: AddCommunicationToCaseResponse
addCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcrResult = Nothing
    }

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
actcrResult :: Lens' AddCommunicationToCaseResponse (Maybe Bool)
actcrResult = lens _actcrResult (\s a -> s { _actcrResult = a })

instance FromJSON AddCommunicationToCaseResponse

instance AWSRequest AddCommunicationToCase where
    type Sv AddCommunicationToCase = Support
    type Rs AddCommunicationToCase = AddCommunicationToCaseResponse

    request = get
    response _ = jsonResponse
