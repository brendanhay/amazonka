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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a customer master key (CMK). To see the decription of a CMK, use 'DescribeKey' .
--
--
-- You cannot perform this operation on a CMK in a different AWS account.
--
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

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateKeyDescription' smart constructor.
data UpdateKeyDescription = UpdateKeyDescription'
  { _ukdKeyId       :: !Text
  , _ukdDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateKeyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ukdKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'ukdDescription' - New description for the CMK.
updateKeyDescription
    :: Text -- ^ 'ukdKeyId'
    -> Text -- ^ 'ukdDescription'
    -> UpdateKeyDescription
updateKeyDescription pKeyId_ pDescription_ =
  UpdateKeyDescription' {_ukdKeyId = pKeyId_, _ukdDescription = pDescription_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
ukdKeyId :: Lens' UpdateKeyDescription Text
ukdKeyId = lens _ukdKeyId (\ s a -> s{_ukdKeyId = a})

-- | New description for the CMK.
ukdDescription :: Lens' UpdateKeyDescription Text
ukdDescription = lens _ukdDescription (\ s a -> s{_ukdDescription = a})

instance AWSRequest UpdateKeyDescription where
        type Rs UpdateKeyDescription =
             UpdateKeyDescriptionResponse
        request = postJSON kms
        response = receiveNull UpdateKeyDescriptionResponse'

instance Hashable UpdateKeyDescription where

instance NFData UpdateKeyDescription where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateKeyDescriptionResponse' with the minimum fields required to make a request.
--
updateKeyDescriptionResponse
    :: UpdateKeyDescriptionResponse
updateKeyDescriptionResponse = UpdateKeyDescriptionResponse'


instance NFData UpdateKeyDescriptionResponse where
