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
-- Module      : Network.AWS.KMS.CancelKeyDeletion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deletion of a customer master key (CMK). When this operation is successful, the CMK is set to the @Disabled@ state. To enable a CMK, use 'EnableKey' . You cannot perform this operation on a CMK in a different AWS account.
--
--
-- For more information about scheduling and canceling deletion of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
--
module Network.AWS.KMS.CancelKeyDeletion
    (
    -- * Creating a Request
      cancelKeyDeletion
    , CancelKeyDeletion
    -- * Request Lenses
    , ckdKeyId

    -- * Destructuring the Response
    , cancelKeyDeletionResponse
    , CancelKeyDeletionResponse
    -- * Response Lenses
    , ckdrsKeyId
    , ckdrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelKeyDeletion' smart constructor.
newtype CancelKeyDeletion = CancelKeyDeletion'
  { _ckdKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelKeyDeletion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckdKeyId' - The unique identifier for the customer master key (CMK) for which to cancel deletion. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
cancelKeyDeletion
    :: Text -- ^ 'ckdKeyId'
    -> CancelKeyDeletion
cancelKeyDeletion pKeyId_ = CancelKeyDeletion' {_ckdKeyId = pKeyId_}


-- | The unique identifier for the customer master key (CMK) for which to cancel deletion. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
ckdKeyId :: Lens' CancelKeyDeletion Text
ckdKeyId = lens _ckdKeyId (\ s a -> s{_ckdKeyId = a})

instance AWSRequest CancelKeyDeletion where
        type Rs CancelKeyDeletion = CancelKeyDeletionResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 CancelKeyDeletionResponse' <$>
                   (x .?> "KeyId") <*> (pure (fromEnum s)))

instance Hashable CancelKeyDeletion where

instance NFData CancelKeyDeletion where

instance ToHeaders CancelKeyDeletion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CancelKeyDeletion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelKeyDeletion where
        toJSON CancelKeyDeletion'{..}
          = object (catMaybes [Just ("KeyId" .= _ckdKeyId)])

instance ToPath CancelKeyDeletion where
        toPath = const "/"

instance ToQuery CancelKeyDeletion where
        toQuery = const mempty

-- | /See:/ 'cancelKeyDeletionResponse' smart constructor.
data CancelKeyDeletionResponse = CancelKeyDeletionResponse'
  { _ckdrsKeyId          :: !(Maybe Text)
  , _ckdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelKeyDeletionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckdrsKeyId' - The unique identifier of the master key for which deletion is canceled.
--
-- * 'ckdrsResponseStatus' - -- | The response status code.
cancelKeyDeletionResponse
    :: Int -- ^ 'ckdrsResponseStatus'
    -> CancelKeyDeletionResponse
cancelKeyDeletionResponse pResponseStatus_ =
  CancelKeyDeletionResponse'
    {_ckdrsKeyId = Nothing, _ckdrsResponseStatus = pResponseStatus_}


-- | The unique identifier of the master key for which deletion is canceled.
ckdrsKeyId :: Lens' CancelKeyDeletionResponse (Maybe Text)
ckdrsKeyId = lens _ckdrsKeyId (\ s a -> s{_ckdrsKeyId = a})

-- | -- | The response status code.
ckdrsResponseStatus :: Lens' CancelKeyDeletionResponse Int
ckdrsResponseStatus = lens _ckdrsResponseStatus (\ s a -> s{_ckdrsResponseStatus = a})

instance NFData CancelKeyDeletionResponse where
