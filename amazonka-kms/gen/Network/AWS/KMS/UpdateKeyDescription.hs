{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateKeyDescription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateKeyDescription.html>
module Network.AWS.KMS.UpdateKeyDescription
    (
    -- * Request
      UpdateKeyDescription
    -- ** Request constructor
    , updateKeyDescription
    -- ** Request lenses
    , ukdrqKeyId
    , ukdrqDescription

    -- * Response
    , UpdateKeyDescriptionResponse
    -- ** Response constructor
    , updateKeyDescriptionResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateKeyDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ukdrqKeyId'
--
-- * 'ukdrqDescription'
data UpdateKeyDescription = UpdateKeyDescription'
    { _ukdrqKeyId       :: !Text
    , _ukdrqDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateKeyDescription' smart constructor.
updateKeyDescription :: Text -> Text -> UpdateKeyDescription
updateKeyDescription pKeyId pDescription =
    UpdateKeyDescription'
    { _ukdrqKeyId = pKeyId
    , _ukdrqDescription = pDescription
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
ukdrqKeyId :: Lens' UpdateKeyDescription Text
ukdrqKeyId = lens _ukdrqKeyId (\ s a -> s{_ukdrqKeyId = a});

-- | New description for the key.
ukdrqDescription :: Lens' UpdateKeyDescription Text
ukdrqDescription = lens _ukdrqDescription (\ s a -> s{_ukdrqDescription = a});

instance AWSRequest UpdateKeyDescription where
        type Sv UpdateKeyDescription = KMS
        type Rs UpdateKeyDescription =
             UpdateKeyDescriptionResponse
        request = postJSON
        response = receiveNull UpdateKeyDescriptionResponse'

instance ToHeaders UpdateKeyDescription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.UpdateKeyDescription" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateKeyDescription where
        toJSON UpdateKeyDescription'{..}
          = object
              ["KeyId" .= _ukdrqKeyId,
               "Description" .= _ukdrqDescription]

instance ToPath UpdateKeyDescription where
        toPath = const "/"

instance ToQuery UpdateKeyDescription where
        toQuery = const mempty

-- | /See:/ 'updateKeyDescriptionResponse' smart constructor.
data UpdateKeyDescriptionResponse =
    UpdateKeyDescriptionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateKeyDescriptionResponse' smart constructor.
updateKeyDescriptionResponse :: UpdateKeyDescriptionResponse
updateKeyDescriptionResponse = UpdateKeyDescriptionResponse'
