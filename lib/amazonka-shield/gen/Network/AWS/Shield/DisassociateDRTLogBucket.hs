{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateDRTLogBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team's (DRT) access to the specified Amazon S3 bucket containing your AWS WAF logs.
--
--
-- To make a @DisassociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTLogBucket@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTLogBucket
  ( -- * Creating a Request
    disassociateDRTLogBucket,
    DisassociateDRTLogBucket,

    -- * Request Lenses
    ddrtlbLogBucket,

    -- * Destructuring the Response
    disassociateDRTLogBucketResponse,
    DisassociateDRTLogBucketResponse,

    -- * Response Lenses
    ddrtlbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'disassociateDRTLogBucket' smart constructor.
newtype DisassociateDRTLogBucket = DisassociateDRTLogBucket'
  { _ddrtlbLogBucket ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateDRTLogBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrtlbLogBucket' - The Amazon S3 bucket that contains your AWS WAF logs.
disassociateDRTLogBucket ::
  -- | 'ddrtlbLogBucket'
  Text ->
  DisassociateDRTLogBucket
disassociateDRTLogBucket pLogBucket_ =
  DisassociateDRTLogBucket' {_ddrtlbLogBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
ddrtlbLogBucket :: Lens' DisassociateDRTLogBucket Text
ddrtlbLogBucket = lens _ddrtlbLogBucket (\s a -> s {_ddrtlbLogBucket = a})

instance AWSRequest DisassociateDRTLogBucket where
  type Rs DisassociateDRTLogBucket = DisassociateDRTLogBucketResponse
  request = postJSON shield
  response =
    receiveEmpty
      ( \s h x ->
          DisassociateDRTLogBucketResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisassociateDRTLogBucket

instance NFData DisassociateDRTLogBucket

instance ToHeaders DisassociateDRTLogBucket where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.DisassociateDRTLogBucket" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateDRTLogBucket where
  toJSON DisassociateDRTLogBucket' {..} =
    object (catMaybes [Just ("LogBucket" .= _ddrtlbLogBucket)])

instance ToPath DisassociateDRTLogBucket where
  toPath = const "/"

instance ToQuery DisassociateDRTLogBucket where
  toQuery = const mempty

-- | /See:/ 'disassociateDRTLogBucketResponse' smart constructor.
newtype DisassociateDRTLogBucketResponse = DisassociateDRTLogBucketResponse'
  { _ddrtlbrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateDRTLogBucketResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrtlbrsResponseStatus' - -- | The response status code.
disassociateDRTLogBucketResponse ::
  -- | 'ddrtlbrsResponseStatus'
  Int ->
  DisassociateDRTLogBucketResponse
disassociateDRTLogBucketResponse pResponseStatus_ =
  DisassociateDRTLogBucketResponse'
    { _ddrtlbrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddrtlbrsResponseStatus :: Lens' DisassociateDRTLogBucketResponse Int
ddrtlbrsResponseStatus = lens _ddrtlbrsResponseStatus (\s a -> s {_ddrtlbrsResponseStatus = a})

instance NFData DisassociateDRTLogBucketResponse
