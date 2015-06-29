{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
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

-- | Decodes additional information about the authorization status of a
-- request from an encoded message returned in response to an AWS request.
--
-- For example, if a user is not authorized to perform an action that he or
-- she has requested, the request returns a @Client.UnauthorizedOperation@
-- response (an HTTP 403 response). Some AWS actions additionally return an
-- encoded message that can provide details about this authorization
-- failure.
--
-- Only certain AWS actions return an encoded authorization message. The
-- documentation for an individual action indicates whether that action
-- returns an encoded message in addition to returning an HTTP code.
--
-- The message is encoded because the details of the authorization status
-- can constitute privileged information that the user who requested the
-- action should not see. To decode an authorization status message, a user
-- must be granted permissions via an IAM policy to request the
-- @DecodeAuthorizationMessage@ (@sts:DecodeAuthorizationMessage@) action.
--
-- The decoded message includes the following type of information:
--
-- -   Whether the request was denied due to an explicit deny or due to the
--     absence of an explicit allow. For more information, see
--     <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccessPolicyLanguage_EvaluationLogic.html#policy-eval-denyallow Determining Whether a Request is Allowed or Denied>
--     in /Using IAM/.
-- -   The principal who made the request.
-- -   The requested action.
-- -   The requested resource.
-- -   The values of condition keys in the context of the user\'s request.
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
    , damrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types

-- | /See:/ 'decodeAuthorizationMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damEncodedMessage'
newtype DecodeAuthorizationMessage = DecodeAuthorizationMessage'
    { _damEncodedMessage :: Text
    } deriving (Eq,Read,Show)

-- | 'DecodeAuthorizationMessage' smart constructor.
decodeAuthorizationMessage :: Text -> DecodeAuthorizationMessage
decodeAuthorizationMessage pEncodedMessage =
    DecodeAuthorizationMessage'
    { _damEncodedMessage = pEncodedMessage
    }

-- | The encoded message that was returned with the response.
damEncodedMessage :: Lens' DecodeAuthorizationMessage Text
damEncodedMessage = lens _damEncodedMessage (\ s a -> s{_damEncodedMessage = a});

instance AWSRequest DecodeAuthorizationMessage where
        type Sv DecodeAuthorizationMessage = STS
        type Rs DecodeAuthorizationMessage =
             DecodeAuthorizationMessageResponse
        request = post
        response
          = receiveXMLWrapper
              "DecodeAuthorizationMessageResult"
              (\ s h x ->
                 DecodeAuthorizationMessageResponse' <$>
                   (x .@? "DecodedMessage") <*> (pure (fromEnum s)))

instance ToHeaders DecodeAuthorizationMessage where
        toHeaders = const mempty

instance ToPath DecodeAuthorizationMessage where
        toPath = const "/"

instance ToQuery DecodeAuthorizationMessage where
        toQuery DecodeAuthorizationMessage'{..}
          = mconcat
              ["Action" =:
                 ("DecodeAuthorizationMessage" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "EncodedMessage" =: _damEncodedMessage]

-- | A document that contains additional information about the authorization
-- status of a request from an encoded message that is returned in response
-- to an AWS request.
--
-- /See:/ 'decodeAuthorizationMessageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damrDecodedMessage'
--
-- * 'damrStatus'
data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse'
    { _damrDecodedMessage :: !(Maybe Text)
    , _damrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'DecodeAuthorizationMessageResponse' smart constructor.
decodeAuthorizationMessageResponse :: Int -> DecodeAuthorizationMessageResponse
decodeAuthorizationMessageResponse pStatus =
    DecodeAuthorizationMessageResponse'
    { _damrDecodedMessage = Nothing
    , _damrStatus = pStatus
    }

-- | An XML document that contains the decoded message. For more information,
-- see @DecodeAuthorizationMessage@.
damrDecodedMessage :: Lens' DecodeAuthorizationMessageResponse (Maybe Text)
damrDecodedMessage = lens _damrDecodedMessage (\ s a -> s{_damrDecodedMessage = a});

-- | FIXME: Undocumented member.
damrStatus :: Lens' DecodeAuthorizationMessageResponse Int
damrStatus = lens _damrStatus (\ s a -> s{_damrStatus = a});
