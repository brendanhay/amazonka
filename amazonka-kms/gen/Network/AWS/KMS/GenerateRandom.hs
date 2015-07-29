{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates an unpredictable byte string.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateRandom.html>
module Network.AWS.KMS.GenerateRandom
    (
    -- * Request
      GenerateRandom
    -- ** Request constructor
    , generateRandom
    -- ** Request lenses
    , grNumberOfBytes

    -- * Response
    , GenerateRandomResponse
    -- ** Response constructor
    , generateRandomResponse
    -- ** Response lenses
    , grrsPlaintext
    , grrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateRandom' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grNumberOfBytes'
newtype GenerateRandom = GenerateRandom'
    { _grNumberOfBytes :: Maybe Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateRandom' smart constructor.
generateRandom :: GenerateRandom
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
        toPath = const mempty

instance ToQuery GenerateRandom where
        toQuery = const mempty

-- | /See:/ 'generateRandomResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrsPlaintext'
--
-- * 'grrsStatus'
data GenerateRandomResponse = GenerateRandomResponse'
    { _grrsPlaintext :: !(Maybe (Sensitive Base64))
    , _grrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateRandomResponse' smart constructor.
generateRandomResponse :: Int -> GenerateRandomResponse
generateRandomResponse pStatus_ =
    GenerateRandomResponse'
    { _grrsPlaintext = Nothing
    , _grrsStatus = pStatus_
    }

-- | Plaintext that contains the unpredictable byte string.
grrsPlaintext :: Lens' GenerateRandomResponse (Maybe ByteString)
grrsPlaintext = lens _grrsPlaintext (\ s a -> s{_grrsPlaintext = a}) . mapping (_Sensitive . _Base64);

-- | FIXME: Undocumented member.
grrsStatus :: Lens' GenerateRandomResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});
