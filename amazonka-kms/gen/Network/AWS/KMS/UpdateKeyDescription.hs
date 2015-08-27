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
-- Module      : Network.AWS.KMS.UpdateKeyDescription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateKeyDescription.html AWS API Reference> for UpdateKeyDescription.
module Network.AWS.KMS.UpdateKeyDescription
    (
    -- * Creating a Request
      updateKeyDescription
    , UpdateKeyDescription
    -- * Request Lenses
    , ukdKeyId
    , ukdDescription

    -- * Destructuring the Response
    , updateKeyDescriptionResponse
    , UpdateKeyDescriptionResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateKeyDescription' smart constructor.
data UpdateKeyDescription = UpdateKeyDescription'
    { _ukdKeyId       :: !Text
    , _ukdDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateKeyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ukdKeyId'
--
-- * 'ukdDescription'
updateKeyDescription
    :: Text -- ^ 'ukdKeyId'
    -> Text -- ^ 'ukdDescription'
    -> UpdateKeyDescription
updateKeyDescription pKeyId_ pDescription_ =
    UpdateKeyDescription'
    { _ukdKeyId = pKeyId_
    , _ukdDescription = pDescription_
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
ukdKeyId :: Lens' UpdateKeyDescription Text
ukdKeyId = lens _ukdKeyId (\ s a -> s{_ukdKeyId = a});

-- | New description for the key.
ukdDescription :: Lens' UpdateKeyDescription Text
ukdDescription = lens _ukdDescription (\ s a -> s{_ukdDescription = a});

instance AWSRequest UpdateKeyDescription where
        type Rs UpdateKeyDescription =
             UpdateKeyDescriptionResponse
        request = postJSON kMS
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
              (catMaybes
                 [Just ("KeyId" .= _ukdKeyId),
                  Just ("Description" .= _ukdDescription)])

instance ToPath UpdateKeyDescription where
        toPath = const "/"

instance ToQuery UpdateKeyDescription where
        toQuery = const mempty

-- | /See:/ 'updateKeyDescriptionResponse' smart constructor.
data UpdateKeyDescriptionResponse =
    UpdateKeyDescriptionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateKeyDescriptionResponse' with the minimum fields required to make a request.
--
updateKeyDescriptionResponse
    :: UpdateKeyDescriptionResponse
updateKeyDescriptionResponse = UpdateKeyDescriptionResponse'
