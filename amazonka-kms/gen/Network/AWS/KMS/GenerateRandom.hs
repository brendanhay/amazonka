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
-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a random byte string that is cryptographically secure.
--
--
-- For more information about entropy and random number generation, see the <https://d0.awsstatic.com/whitepapers/KMS-Cryptographic-Details.pdf AWS Key Management Service Cryptographic Details> whitepaper.
--
module Network.AWS.KMS.GenerateRandom
    (
    -- * Creating a Request
      generateRandom
    , GenerateRandom
    -- * Request Lenses
    , grNumberOfBytes

    -- * Destructuring the Response
    , generateRandomResponse
    , GenerateRandomResponse
    -- * Response Lenses
    , grrsPlaintext
    , grrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateRandom' smart constructor.
newtype GenerateRandom = GenerateRandom'
  { _grNumberOfBytes :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateRandom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grNumberOfBytes' - The length of the byte string.
generateRandom
    :: GenerateRandom
generateRandom = GenerateRandom' {_grNumberOfBytes = Nothing}


-- | The length of the byte string.
grNumberOfBytes :: Lens' GenerateRandom (Maybe Natural)
grNumberOfBytes = lens _grNumberOfBytes (\ s a -> s{_grNumberOfBytes = a}) . mapping _Nat

instance AWSRequest GenerateRandom where
        type Rs GenerateRandom = GenerateRandomResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GenerateRandomResponse' <$>
                   (x .?> "Plaintext") <*> (pure (fromEnum s)))

instance Hashable GenerateRandom where

instance NFData GenerateRandom where

instance ToHeaders GenerateRandom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GenerateRandom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateRandom where
        toJSON GenerateRandom'{..}
          = object
              (catMaybes
                 [("NumberOfBytes" .=) <$> _grNumberOfBytes])

instance ToPath GenerateRandom where
        toPath = const "/"

instance ToQuery GenerateRandom where
        toQuery = const mempty

-- | /See:/ 'generateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { _grrsPlaintext      :: !(Maybe (Sensitive Base64))
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateRandomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsPlaintext' - The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'grrsResponseStatus' - -- | The response status code.
generateRandomResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GenerateRandomResponse
generateRandomResponse pResponseStatus_ =
  GenerateRandomResponse'
    {_grrsPlaintext = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
grrsPlaintext :: Lens' GenerateRandomResponse (Maybe ByteString)
grrsPlaintext = lens _grrsPlaintext (\ s a -> s{_grrsPlaintext = a}) . mapping (_Sensitive . _Base64)

-- | -- | The response status code.
grrsResponseStatus :: Lens' GenerateRandomResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GenerateRandomResponse where
