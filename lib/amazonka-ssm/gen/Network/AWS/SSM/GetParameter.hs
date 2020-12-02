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
-- Module      : Network.AWS.SSM.GetParameter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter by using the parameter name.
--
--
module Network.AWS.SSM.GetParameter
    (
    -- * Creating a Request
      getParameter
    , GetParameter
    -- * Request Lenses
    , gWithDecryption
    , gName

    -- * Destructuring the Response
    , getParameterResponse
    , GetParameterResponse
    -- * Response Lenses
    , gprsParameter
    , gprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getParameter' smart constructor.
data GetParameter = GetParameter'
  { _gWithDecryption :: !(Maybe Bool)
  , _gName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gWithDecryption' - Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- * 'gName' - The name of the parameter you want to query.
getParameter
    :: Text -- ^ 'gName'
    -> GetParameter
getParameter pName_ =
  GetParameter' {_gWithDecryption = Nothing, _gName = pName_}


-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
gWithDecryption :: Lens' GetParameter (Maybe Bool)
gWithDecryption = lens _gWithDecryption (\ s a -> s{_gWithDecryption = a})

-- | The name of the parameter you want to query.
gName :: Lens' GetParameter Text
gName = lens _gName (\ s a -> s{_gName = a})

instance AWSRequest GetParameter where
        type Rs GetParameter = GetParameterResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetParameterResponse' <$>
                   (x .?> "Parameter") <*> (pure (fromEnum s)))

instance Hashable GetParameter where

instance NFData GetParameter where

instance ToHeaders GetParameter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetParameter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetParameter where
        toJSON GetParameter'{..}
          = object
              (catMaybes
                 [("WithDecryption" .=) <$> _gWithDecryption,
                  Just ("Name" .= _gName)])

instance ToPath GetParameter where
        toPath = const "/"

instance ToQuery GetParameter where
        toQuery = const mempty

-- | /See:/ 'getParameterResponse' smart constructor.
data GetParameterResponse = GetParameterResponse'
  { _gprsParameter      :: !(Maybe Parameter)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsParameter' - Information about a parameter.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getParameterResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetParameterResponse
getParameterResponse pResponseStatus_ =
  GetParameterResponse'
    {_gprsParameter = Nothing, _gprsResponseStatus = pResponseStatus_}


-- | Information about a parameter.
gprsParameter :: Lens' GetParameterResponse (Maybe Parameter)
gprsParameter = lens _gprsParameter (\ s a -> s{_gprsParameter = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetParameterResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetParameterResponse where
