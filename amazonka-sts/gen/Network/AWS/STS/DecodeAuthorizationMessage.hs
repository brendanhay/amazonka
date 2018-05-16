{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decodes additional information about the authorization status of a request from an encoded message returned in response to an AWS request.
--
--
-- For example, if a user is not authorized to perform an action that he or she has requested, the request returns a @Client.UnauthorizedOperation@ response (an HTTP 403 response). Some AWS actions additionally return an encoded message that can provide details about this authorization failure.
--
-- The message is encoded because the details of the authorization status can constitute privileged information that the user who requested the action should not see. To decode an authorization status message, a user must be granted permissions via an IAM policy to request the @DecodeAuthorizationMessage@ (@sts:DecodeAuthorizationMessage@ ) action.
--
-- The decoded message includes the following type of information:
--
--     * Whether the request was denied due to an explicit deny or due to the absence of an explicit allow. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-denyallow Determining Whether a Request is Allowed or Denied> in the /IAM User Guide/ .
--
--     * The principal who made the request.
--
--     * The requested action.
--
--     * The requested resource.
--
--     * The values of condition keys in the context of the user's request.
--
--
--
module Network.AWS.STS.DecodeAuthorizationMessage
    (
    -- * Creating a Request
      decodeAuthorizationMessage
    , DecodeAuthorizationMessage
    -- * Request Lenses
    , damEncodedMessage

    -- * Destructuring the Response
    , decodeAuthorizationMessageResponse
    , DecodeAuthorizationMessageResponse
    -- * Response Lenses
    , damrsDecodedMessage
    , damrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types
import Network.AWS.STS.Types.Product

-- | /See:/ 'decodeAuthorizationMessage' smart constructor.
newtype DecodeAuthorizationMessage = DecodeAuthorizationMessage'
  { _damEncodedMessage :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecodeAuthorizationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damEncodedMessage' - The encoded message that was returned with the response.
decodeAuthorizationMessage
    :: Text -- ^ 'damEncodedMessage'
    -> DecodeAuthorizationMessage
decodeAuthorizationMessage pEncodedMessage_ =
  DecodeAuthorizationMessage' {_damEncodedMessage = pEncodedMessage_}


-- | The encoded message that was returned with the response.
damEncodedMessage :: Lens' DecodeAuthorizationMessage Text
damEncodedMessage = lens _damEncodedMessage (\ s a -> s{_damEncodedMessage = a})

instance AWSRequest DecodeAuthorizationMessage where
        type Rs DecodeAuthorizationMessage =
             DecodeAuthorizationMessageResponse
        request = postQuery sts
        response
          = receiveXMLWrapper
              "DecodeAuthorizationMessageResult"
              (\ s h x ->
                 DecodeAuthorizationMessageResponse' <$>
                   (x .@? "DecodedMessage") <*> (pure (fromEnum s)))

instance Hashable DecodeAuthorizationMessage where

instance NFData DecodeAuthorizationMessage where

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

-- | A document that contains additional information about the authorization status of a request from an encoded message that is returned in response to an AWS request.
--
--
--
-- /See:/ 'decodeAuthorizationMessageResponse' smart constructor.
data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse'
  { _damrsDecodedMessage :: !(Maybe Text)
  , _damrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecodeAuthorizationMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damrsDecodedMessage' - An XML document that contains the decoded message.
--
-- * 'damrsResponseStatus' - -- | The response status code.
decodeAuthorizationMessageResponse
    :: Int -- ^ 'damrsResponseStatus'
    -> DecodeAuthorizationMessageResponse
decodeAuthorizationMessageResponse pResponseStatus_ =
  DecodeAuthorizationMessageResponse'
    {_damrsDecodedMessage = Nothing, _damrsResponseStatus = pResponseStatus_}


-- | An XML document that contains the decoded message.
damrsDecodedMessage :: Lens' DecodeAuthorizationMessageResponse (Maybe Text)
damrsDecodedMessage = lens _damrsDecodedMessage (\ s a -> s{_damrsDecodedMessage = a})

-- | -- | The response status code.
damrsResponseStatus :: Lens' DecodeAuthorizationMessageResponse Int
damrsResponseStatus = lens _damrsResponseStatus (\ s a -> s{_damrsResponseStatus = a})

instance NFData DecodeAuthorizationMessageResponse
         where
