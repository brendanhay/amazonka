{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , actcAttachmentSetId
    , actcCaseId
    , actcCcEmailAddresses
    , actcCommunicationBody

    -- * Response
    , AddCommunicationToCaseResponse
    -- ** Response constructor
    , addCommunicationToCaseResponse
    -- ** Response lenses
    , actcrResult
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Support.Types

data AddCommunicationToCase = AddCommunicationToCase
    { _actcAttachmentSetId   :: Maybe Text
    , _actcCaseId            :: Maybe Text
    , _actcCcEmailAddresses  :: [Text]
    , _actcCommunicationBody :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddCommunicationToCase' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actcAttachmentSetId' @::@ 'Maybe' 'Text'
--
-- * 'actcCaseId' @::@ 'Maybe' 'Text'
--
-- * 'actcCcEmailAddresses' @::@ ['Text']
--
-- * 'actcCommunicationBody' @::@ 'Text'
--
addCommunicationToCase :: Text -- ^ 'actcCommunicationBody'
                       -> AddCommunicationToCase
addCommunicationToCase p1 = AddCommunicationToCase
    { _actcCommunicationBody = p1
    , _actcCaseId            = Nothing
    , _actcCcEmailAddresses  = mempty
    , _actcAttachmentSetId   = Nothing
    }

-- | The ID of a set of one or more attachments for the communication to add
-- to the case. Create the set by calling AddAttachmentsToSet.
actcAttachmentSetId :: Lens' AddCommunicationToCase (Maybe Text)
actcAttachmentSetId =
    lens _actcAttachmentSetId (\s a -> s { _actcAttachmentSetId = a })

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
actcCaseId :: Lens' AddCommunicationToCase (Maybe Text)
actcCaseId = lens _actcCaseId (\s a -> s { _actcCaseId = a })

-- | The email addresses in the CC line of an email to be added to the support
-- case.
actcCcEmailAddresses :: Lens' AddCommunicationToCase [Text]
actcCcEmailAddresses =
    lens _actcCcEmailAddresses (\s a -> s { _actcCcEmailAddresses = a })

-- | The body of an email communication to add to the support case.
actcCommunicationBody :: Lens' AddCommunicationToCase Text
actcCommunicationBody =
    lens _actcCommunicationBody (\s a -> s { _actcCommunicationBody = a })

instance ToPath AddCommunicationToCase where
    toPath = const "/"

instance ToQuery AddCommunicationToCase where
    toQuery = const mempty

instance ToHeaders AddCommunicationToCase

instance ToBody AddCommunicationToCase where
    toBody = toBody . encode . _actcCaseId

newtype AddCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcrResult :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddCommunicationToCaseResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actcrResult' @::@ 'Maybe' 'Bool'
--
addCommunicationToCaseResponse :: AddCommunicationToCaseResponse
addCommunicationToCaseResponse = AddCommunicationToCaseResponse
    { _actcrResult = Nothing
    }

-- | True if AddCommunicationToCase succeeds. Otherwise, returns an error.
actcrResult :: Lens' AddCommunicationToCaseResponse (Maybe Bool)
actcrResult = lens _actcrResult (\s a -> s { _actcrResult = a })

-- FromJSON

instance AWSRequest AddCommunicationToCase where
    type Sv AddCommunicationToCase = Support
    type Rs AddCommunicationToCase = AddCommunicationToCaseResponse

    request  = post'
    response = jsonResponse $ \h o -> AddCommunicationToCaseResponse
        <$> o .: "result"
