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
-- Module      : Network.AWS.SNS.OptInPhoneNumber
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to opt in a phone number that is opted out, which enables you to resume sending SMS messages to the number.
--
--
-- You can opt in a phone number only once every 30 days.
--
module Network.AWS.SNS.OptInPhoneNumber
    (
    -- * Creating a Request
      optInPhoneNumber
    , OptInPhoneNumber
    -- * Request Lenses
    , oipnPhoneNumber

    -- * Destructuring the Response
    , optInPhoneNumberResponse
    , OptInPhoneNumberResponse
    -- * Response Lenses
    , oipnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for the OptInPhoneNumber action.
--
--
--
-- /See:/ 'optInPhoneNumber' smart constructor.
newtype OptInPhoneNumber = OptInPhoneNumber'
  { _oipnPhoneNumber :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptInPhoneNumber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oipnPhoneNumber' - The phone number to opt in.
optInPhoneNumber
    :: Text -- ^ 'oipnPhoneNumber'
    -> OptInPhoneNumber
optInPhoneNumber pPhoneNumber_ =
  OptInPhoneNumber' {_oipnPhoneNumber = pPhoneNumber_}


-- | The phone number to opt in.
oipnPhoneNumber :: Lens' OptInPhoneNumber Text
oipnPhoneNumber = lens _oipnPhoneNumber (\ s a -> s{_oipnPhoneNumber = a})

instance AWSRequest OptInPhoneNumber where
        type Rs OptInPhoneNumber = OptInPhoneNumberResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "OptInPhoneNumberResult"
              (\ s h x ->
                 OptInPhoneNumberResponse' <$> (pure (fromEnum s)))

instance Hashable OptInPhoneNumber where

instance NFData OptInPhoneNumber where

instance ToHeaders OptInPhoneNumber where
        toHeaders = const mempty

instance ToPath OptInPhoneNumber where
        toPath = const "/"

instance ToQuery OptInPhoneNumber where
        toQuery OptInPhoneNumber'{..}
          = mconcat
              ["Action" =: ("OptInPhoneNumber" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "phoneNumber" =: _oipnPhoneNumber]

-- | The response for the OptInPhoneNumber action.
--
--
--
-- /See:/ 'optInPhoneNumberResponse' smart constructor.
newtype OptInPhoneNumberResponse = OptInPhoneNumberResponse'
  { _oipnrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptInPhoneNumberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oipnrsResponseStatus' - -- | The response status code.
optInPhoneNumberResponse
    :: Int -- ^ 'oipnrsResponseStatus'
    -> OptInPhoneNumberResponse
optInPhoneNumberResponse pResponseStatus_ =
  OptInPhoneNumberResponse' {_oipnrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
oipnrsResponseStatus :: Lens' OptInPhoneNumberResponse Int
oipnrsResponseStatus = lens _oipnrsResponseStatus (\ s a -> s{_oipnrsResponseStatus = a})

instance NFData OptInPhoneNumberResponse where
