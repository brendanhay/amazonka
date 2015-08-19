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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an unpredictable byte string.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateRandom.html AWS API Reference> for GenerateRandom.
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
    , grrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateRandom' smart constructor.
newtype GenerateRandom = GenerateRandom'
    { _grNumberOfBytes :: Maybe Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateRandom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grNumberOfBytes'
generateRandom
    :: GenerateRandom
generateRandom =
    GenerateRandom'
    { _grNumberOfBytes = Nothing
    }

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on. The current limit is 1024 bytes.
grNumberOfBytes :: Lens' GenerateRandom (Maybe Natural)
grNumberOfBytes = lens _grNumberOfBytes (\ s a -> s{_grNumberOfBytes = a}) . mapping _Nat;

instance AWSRequest GenerateRandom where
        type Sv GenerateRandom = KMS
        type Rs GenerateRandom = GenerateRandomResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GenerateRandomResponse' <$>
                   (x .?> "Plaintext") <*> (pure (fromEnum s)))

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
          = object ["NumberOfBytes" .= _grNumberOfBytes]

instance ToPath GenerateRandom where
        toPath = const "/"

instance ToQuery GenerateRandom where
        toQuery = const mempty

-- | /See:/ 'generateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
    { _grrsPlaintext :: !(Maybe (Sensitive Base64))
    , _grrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateRandomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsPlaintext'
--
-- * 'grrsStatus'
generateRandomResponse
    :: Int -- ^ 'grrsStatus'
    -> GenerateRandomResponse
generateRandomResponse pStatus_ =
    GenerateRandomResponse'
    { _grrsPlaintext = Nothing
    , _grrsStatus = pStatus_
    }

-- | Plaintext that contains the unpredictable byte string.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphim will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
grrsPlaintext :: Lens' GenerateRandomResponse (Maybe ByteString)
grrsPlaintext = lens _grrsPlaintext (\ s a -> s{_grrsPlaintext = a}) . mapping (_Sensitive . _Base64);

-- | The response status code.
grrsStatus :: Lens' GenerateRandomResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});
