{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the visibility timeout of multiple messages. This is a batch
-- version of ChangeMessageVisibility. The result of the action on each
-- message is reported individually in the response. You can send up to 10
-- ChangeMessageVisibility requests with each ChangeMessageVisibilityBatch
-- action. Because the batch request can result in a combination of successful
-- and unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of 200. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that
-- ChangeMessageVisibilityBatch request changes the visibility timeout
-- settings for two messages. You must URL encode the entire URL; however,
-- we've URL encoded only the message body to make the example easier for you
-- to read. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- &Action=ChangeMessageVisibilityBatch &Version=2011-10-01
-- &ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2
-- &ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33G4ms5subrE0deLKWSscPU1oD3J9zgeS4PQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOjPcTpRxBtXix%2BEvwJOZUma9wabv%2BSw6ZHjwmNcVDx8dZXJhVp16Bksiox%2FGrUvrVTCJRTWTLc59oHLLF8sEkKzRmGNzTDGTiV%2BYjHfQj60FD3rVaXmzTsoNxRhKJ72uIHVMGVQiAGgBX6HGv9LDmYhPXw4hy%2FNgIg%3D%3D
-- &ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45
-- &ChangeMessageVisibilityBatchRequestEntry.2.Id=change_visibility_msg_3
-- &ChangeMessageVisibilityBatchRequestEntry.2.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33FUgBz3%2BnougdeLKWSscPU1%2FXgx%2BxcNnjnQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOsogjZM%2F7kzn4Ew27XLU9I%2FYaWYmKvDbq%2Fk3HKVB9HfB43kE49atP2aWrzNL4yunG41Q4cfRRtfJdcGQGNHQ2%2Byd0Usf5qR1dZr1iDo5xk946eQat83AxTRP%2BY4Qi0V7FAeSLH9su9xpX6HGv9LDmYhPXw4hy%2FNgIg%3D%3D
-- &ChangeMessageVisibilityBatchRequestEntry.2.VisibilityTimeout=45
-- &SignatureMethod=HmacSHA256 &Expires=2011-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE change_visibility_msg_2
-- change_visibility_msg_3 ca9668f7-ab1b-4f7a-8859-f15747ab17a7.
module Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch
    { _cmvbrEntries :: [ChangeMessageVisibilityBatchRequestEntry]
      -- ^ A list of receipt handles of the messages for which the
      -- visibility timeout must be changed.
    , _cmvbrQueueUrl :: Text
      -- ^ The URL of the Amazon SQS queue to take action on.
    } deriving (Show, Generic)

makeLenses ''ChangeMessageVisibilityBatch

instance ToQuery ChangeMessageVisibilityBatch where
    toQuery = genericQuery def

data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse
    { _cmvbsFailed :: [BatchResultErrorEntry]
      -- ^ A list of BatchResultErrorEntry items.
    , _cmvbsSuccessful :: [ChangeMessageVisibilityBatchResultEntry]
      -- ^ A list of ChangeMessageVisibilityBatchResultEntry items.
    } deriving (Show, Generic)

makeLenses ''ChangeMessageVisibilityBatchResponse

instance FromXML ChangeMessageVisibilityBatchResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ChangeMessageVisibilityBatch where
    type Sv ChangeMessageVisibilityBatch = SQS
    type Rs ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatchResponse

    request = post "ChangeMessageVisibilityBatch"
    response _ = xmlResponse
