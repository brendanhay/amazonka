{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DisableKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Marks a key as disabled, thereby preventing its use.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_DisableKey.html>
module Network.AWS.KMS.DisableKey
    (
    -- * Request
      DisableKey
    -- ** Request constructor
    , disableKey
    -- ** Request lenses
    , dkKeyId

    -- * Response
    , DisableKeyResponse
    -- ** Response constructor
    , disableKeyResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkKeyId'
newtype DisableKey = DisableKey'
    { _dkKeyId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableKey' smart constructor.
disableKey :: Text -> DisableKey
disableKey pKeyId_ =
    DisableKey'
    { _dkKeyId = pKeyId_
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
dkKeyId :: Lens' DisableKey Text
dkKeyId = lens _dkKeyId (\ s a -> s{_dkKeyId = a});

instance AWSRequest DisableKey where
        type Sv DisableKey = KMS
        type Rs DisableKey = DisableKeyResponse
        request = postJSON "DisableKey"
        response = receiveNull DisableKeyResponse'

instance ToHeaders DisableKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DisableKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableKey where
        toJSON DisableKey'{..} = object ["KeyId" .= _dkKeyId]

instance ToPath DisableKey where
        toPath = const "/"

instance ToQuery DisableKey where
        toQuery = const mempty

-- | /See:/ 'disableKeyResponse' smart constructor.
data DisableKeyResponse =
    DisableKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableKeyResponse' smart constructor.
disableKeyResponse :: DisableKeyResponse
disableKeyResponse = DisableKeyResponse'
