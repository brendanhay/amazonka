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

-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Decodes additional information about the authorization status of a request
-- from an encoded message returned in response to an AWS request. For
-- example, if a user is not authorized to perform an action that he or she
-- has requested, the request returns a Client.UnauthorizedOperation response
-- (an HTTP 403 response). Some AWS actions additionally return an encoded
-- message that can provide details about this authorization failure. The
-- message is encoded because the details of the authorization status can
-- constitute privileged information that the user who requested the action
-- should not see. To decode an authorization status message, a user must be
-- granted permissions via an IAM policy to request the
-- DecodeAuthorizationMessage (sts:DecodeAuthorizationMessage) action. The
-- decoded message includes the following type of information: Whether the
-- request was denied due to an explicit deny or due to the absence of an
-- explicit allow. For more information, see Determining Whether a Request is
-- Allowed or Denied in Using IAM. The principal who made the request. The
-- requested action. The requested resource. The values of condition keys in
-- the context of the user's request.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_DecodeAuthorizationMessage.html>
module Network.AWS.STS.DecodeAuthorizationMessage
    (
    -- * Request
      DecodeAuthorizationMessage
    -- ** Request constructor
    , decodeAuthorizationMessage
    -- ** Request lenses
    , damEncodedMessage

    -- * Response
    , DecodeAuthorizationMessageResponse
    -- ** Response constructor
    , decodeAuthorizationMessageResponse
    -- ** Response lenses
    , damrDecodedMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.STS.Types
import qualified GHC.Exts

newtype DecodeAuthorizationMessage = DecodeAuthorizationMessage
    { _damEncodedMessage :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DecodeAuthorizationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damEncodedMessage' @::@ 'Text'
--
decodeAuthorizationMessage :: Text -- ^ 'damEncodedMessage'
                           -> DecodeAuthorizationMessage
decodeAuthorizationMessage p1 = DecodeAuthorizationMessage
    { _damEncodedMessage = p1
    }

-- | The encoded message that was returned with the response.
damEncodedMessage :: Lens' DecodeAuthorizationMessage Text
damEncodedMessage =
    lens _damEncodedMessage (\s a -> s { _damEncodedMessage = a })

newtype DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse
    { _damrDecodedMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DecodeAuthorizationMessageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damrDecodedMessage' @::@ 'Maybe' 'Text'
--
decodeAuthorizationMessageResponse :: DecodeAuthorizationMessageResponse
decodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse
    { _damrDecodedMessage = Nothing
    }

-- | An XML document that contains the decoded message. For more information,
-- see DecodeAuthorizationMessage.
damrDecodedMessage :: Lens' DecodeAuthorizationMessageResponse (Maybe Text)
damrDecodedMessage =
    lens _damrDecodedMessage (\s a -> s { _damrDecodedMessage = a })

instance ToPath DecodeAuthorizationMessage where
    toPath = const "/"

instance ToQuery DecodeAuthorizationMessage where
    toQuery DecodeAuthorizationMessage{..} = mconcat
        [ "EncodedMessage" =? _damEncodedMessage
        ]

instance ToHeaders DecodeAuthorizationMessage

instance AWSRequest DecodeAuthorizationMessage where
    type Sv DecodeAuthorizationMessage = STS
    type Rs DecodeAuthorizationMessage = DecodeAuthorizationMessageResponse

    request  = post "DecodeAuthorizationMessage"
    response = xmlResponse

instance FromXML DecodeAuthorizationMessageResponse where
    parseXML = withElement "DecodeAuthorizationMessageResult" $ \x -> DecodeAuthorizationMessageResponse
        <$> x .@? "DecodedMessage"


Some kind of operator / class to check the types whether to continue?
