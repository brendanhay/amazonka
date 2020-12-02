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
-- Module      : Network.AWS.SSM.GetParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of a parameter.
--
--
module Network.AWS.SSM.GetParameters
    (
    -- * Creating a Request
      getParameters
    , GetParameters
    -- * Request Lenses
    , gpWithDecryption
    , gpNames

    -- * Destructuring the Response
    , getParametersResponse
    , GetParametersResponse
    -- * Response Lenses
    , grsParameters
    , grsInvalidParameters
    , grsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getParameters' smart constructor.
data GetParameters = GetParameters'
  { _gpWithDecryption :: !(Maybe Bool)
  , _gpNames          :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpWithDecryption' - Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- * 'gpNames' - Names of the parameters for which you want to query information.
getParameters
    :: NonEmpty Text -- ^ 'gpNames'
    -> GetParameters
getParameters pNames_ =
  GetParameters' {_gpWithDecryption = Nothing, _gpNames = _List1 # pNames_}


-- | Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
gpWithDecryption :: Lens' GetParameters (Maybe Bool)
gpWithDecryption = lens _gpWithDecryption (\ s a -> s{_gpWithDecryption = a})

-- | Names of the parameters for which you want to query information.
gpNames :: Lens' GetParameters (NonEmpty Text)
gpNames = lens _gpNames (\ s a -> s{_gpNames = a}) . _List1

instance AWSRequest GetParameters where
        type Rs GetParameters = GetParametersResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetParametersResponse' <$>
                   (x .?> "Parameters" .!@ mempty) <*>
                     (x .?> "InvalidParameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetParameters where

instance NFData GetParameters where

instance ToHeaders GetParameters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetParameters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetParameters where
        toJSON GetParameters'{..}
          = object
              (catMaybes
                 [("WithDecryption" .=) <$> _gpWithDecryption,
                  Just ("Names" .= _gpNames)])

instance ToPath GetParameters where
        toPath = const "/"

instance ToQuery GetParameters where
        toQuery = const mempty

-- | /See:/ 'getParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { _grsParameters        :: !(Maybe [Parameter])
  , _grsInvalidParameters :: !(Maybe [Text])
  , _grsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsParameters' - A list of details for a parameter.
--
-- * 'grsInvalidParameters' - A list of parameters that are not formatted correctly or do not run when executed.
--
-- * 'grsResponseStatus' - -- | The response status code.
getParametersResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetParametersResponse
getParametersResponse pResponseStatus_ =
  GetParametersResponse'
    { _grsParameters = Nothing
    , _grsInvalidParameters = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | A list of details for a parameter.
grsParameters :: Lens' GetParametersResponse [Parameter]
grsParameters = lens _grsParameters (\ s a -> s{_grsParameters = a}) . _Default . _Coerce

-- | A list of parameters that are not formatted correctly or do not run when executed.
grsInvalidParameters :: Lens' GetParametersResponse [Text]
grsInvalidParameters = lens _grsInvalidParameters (\ s a -> s{_grsInvalidParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
grsResponseStatus :: Lens' GetParametersResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetParametersResponse where
