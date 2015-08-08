{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.EnableKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Marks a key as enabled, thereby permitting its use. You can have up to
-- 25 enabled keys at one time.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_EnableKey.html AWS API Reference> for EnableKey.
module Network.AWS.KMS.EnableKey
    (
    -- * Creating a Request
      EnableKey
    , enableKey
    -- * Request Lenses
    , ekKeyId

    -- * Destructuring the Response
    , EnableKeyResponse
    , enableKeyResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ekKeyId'
newtype EnableKey = EnableKey'
    { _ekKeyId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableKey' smart constructor.
enableKey :: Text -> EnableKey
enableKey pKeyId_ =
    EnableKey'
    { _ekKeyId = pKeyId_
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
ekKeyId :: Lens' EnableKey Text
ekKeyId = lens _ekKeyId (\ s a -> s{_ekKeyId = a});

instance AWSRequest EnableKey where
        type Sv EnableKey = KMS
        type Rs EnableKey = EnableKeyResponse
        request = postJSON
        response = receiveNull EnableKeyResponse'

instance ToHeaders EnableKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.EnableKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableKey where
        toJSON EnableKey'{..} = object ["KeyId" .= _ekKeyId]

instance ToPath EnableKey where
        toPath = const "/"

instance ToQuery EnableKey where
        toQuery = const mempty

-- | /See:/ 'enableKeyResponse' smart constructor.
data EnableKeyResponse =
    EnableKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableKeyResponse' smart constructor.
enableKeyResponse :: EnableKeyResponse
enableKeyResponse = EnableKeyResponse'
