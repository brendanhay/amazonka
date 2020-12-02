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
-- Module      : Network.AWS.MediaConvert.AssociateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Certificate Manager (ACM) Amazon Resource Name (ARN) with AWS Elemental MediaConvert.
module Network.AWS.MediaConvert.AssociateCertificate
  ( -- * Creating a Request
    associateCertificate,
    AssociateCertificate,

    -- * Request Lenses
    acARN,

    -- * Destructuring the Response
    associateCertificateResponse,
    AssociateCertificateResponse,

    -- * Response Lenses
    acrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateCertificate' smart constructor.
newtype AssociateCertificate = AssociateCertificate'
  { _acARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acARN' - The ARN of the ACM certificate that you want to associate with your MediaConvert resource.
associateCertificate ::
  -- | 'acARN'
  Text ->
  AssociateCertificate
associateCertificate pARN_ = AssociateCertificate' {_acARN = pARN_}

-- | The ARN of the ACM certificate that you want to associate with your MediaConvert resource.
acARN :: Lens' AssociateCertificate Text
acARN = lens _acARN (\s a -> s {_acARN = a})

instance AWSRequest AssociateCertificate where
  type Rs AssociateCertificate = AssociateCertificateResponse
  request = postJSON mediaConvert
  response =
    receiveEmpty
      (\s h x -> AssociateCertificateResponse' <$> (pure (fromEnum s)))

instance Hashable AssociateCertificate

instance NFData AssociateCertificate

instance ToHeaders AssociateCertificate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateCertificate where
  toJSON AssociateCertificate' {..} =
    object (catMaybes [Just ("arn" .= _acARN)])

instance ToPath AssociateCertificate where
  toPath = const "/2017-08-29/certificates"

instance ToQuery AssociateCertificate where
  toQuery = const mempty

-- | /See:/ 'associateCertificateResponse' smart constructor.
newtype AssociateCertificateResponse = AssociateCertificateResponse'
  { _acrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrsResponseStatus' - -- | The response status code.
associateCertificateResponse ::
  -- | 'acrsResponseStatus'
  Int ->
  AssociateCertificateResponse
associateCertificateResponse pResponseStatus_ =
  AssociateCertificateResponse'
    { _acrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
acrsResponseStatus :: Lens' AssociateCertificateResponse Int
acrsResponseStatus = lens _acrsResponseStatus (\s a -> s {_acrsResponseStatus = a})

instance NFData AssociateCertificateResponse
