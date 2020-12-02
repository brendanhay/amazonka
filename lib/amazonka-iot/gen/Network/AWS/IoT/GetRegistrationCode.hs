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
-- Module      : Network.AWS.IoT.GetRegistrationCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a registration code used to register a CA certificate with AWS IoT.
--
--
module Network.AWS.IoT.GetRegistrationCode
    (
    -- * Creating a Request
      getRegistrationCode
    , GetRegistrationCode

    -- * Destructuring the Response
    , getRegistrationCodeResponse
    , GetRegistrationCodeResponse
    -- * Response Lenses
    , grcrsRegistrationCode
    , grcrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the GetRegistrationCode operation.
--
--
--
-- /See:/ 'getRegistrationCode' smart constructor.
data GetRegistrationCode =
  GetRegistrationCode'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegistrationCode' with the minimum fields required to make a request.
--
getRegistrationCode
    :: GetRegistrationCode
getRegistrationCode = GetRegistrationCode'


instance AWSRequest GetRegistrationCode where
        type Rs GetRegistrationCode =
             GetRegistrationCodeResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetRegistrationCodeResponse' <$>
                   (x .?> "registrationCode") <*> (pure (fromEnum s)))

instance Hashable GetRegistrationCode where

instance NFData GetRegistrationCode where

instance ToHeaders GetRegistrationCode where
        toHeaders = const mempty

instance ToPath GetRegistrationCode where
        toPath = const "/registrationcode"

instance ToQuery GetRegistrationCode where
        toQuery = const mempty

-- | The output from the GetRegistrationCode operation.
--
--
--
-- /See:/ 'getRegistrationCodeResponse' smart constructor.
data GetRegistrationCodeResponse = GetRegistrationCodeResponse'
  { _grcrsRegistrationCode :: !(Maybe Text)
  , _grcrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegistrationCodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcrsRegistrationCode' - The CA certificate registration code.
--
-- * 'grcrsResponseStatus' - -- | The response status code.
getRegistrationCodeResponse
    :: Int -- ^ 'grcrsResponseStatus'
    -> GetRegistrationCodeResponse
getRegistrationCodeResponse pResponseStatus_ =
  GetRegistrationCodeResponse'
    {_grcrsRegistrationCode = Nothing, _grcrsResponseStatus = pResponseStatus_}


-- | The CA certificate registration code.
grcrsRegistrationCode :: Lens' GetRegistrationCodeResponse (Maybe Text)
grcrsRegistrationCode = lens _grcrsRegistrationCode (\ s a -> s{_grcrsRegistrationCode = a})

-- | -- | The response status code.
grcrsResponseStatus :: Lens' GetRegistrationCodeResponse Int
grcrsResponseStatus = lens _grcrsResponseStatus (\ s a -> s{_grcrsResponseStatus = a})

instance NFData GetRegistrationCodeResponse where
