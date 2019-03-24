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
-- Module      : Network.AWS.MediaConvert.DisassociateCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an association between the Amazon Resource Name (ARN) of an AWS Certificate Manager (ACM) certificate and an AWS Elemental MediaConvert resource.
module Network.AWS.MediaConvert.DisassociateCertificate
    (
    -- * Creating a Request
      disassociateCertificate
    , DisassociateCertificate
    -- * Request Lenses
    , dcARN

    -- * Destructuring the Response
    , disassociateCertificateResponse
    , DisassociateCertificateResponse
    -- * Response Lenses
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateCertificate' smart constructor.
newtype DisassociateCertificate = DisassociateCertificate'
  { _dcARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcARN' - The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
disassociateCertificate
    :: Text -- ^ 'dcARN'
    -> DisassociateCertificate
disassociateCertificate pARN_ = DisassociateCertificate' {_dcARN = pARN_}


-- | The ARN of the ACM certificate that you want to disassociate from your MediaConvert resource.
dcARN :: Lens' DisassociateCertificate Text
dcARN = lens _dcARN (\ s a -> s{_dcARN = a})

instance AWSRequest DisassociateCertificate where
        type Rs DisassociateCertificate =
             DisassociateCertificateResponse
        request = delete mediaConvert
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateCertificateResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateCertificate where

instance NFData DisassociateCertificate where

instance ToHeaders DisassociateCertificate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DisassociateCertificate where
        toPath DisassociateCertificate'{..}
          = mconcat ["/2017-08-29/certificates/", toBS _dcARN]

instance ToQuery DisassociateCertificate where
        toQuery = const mempty

-- | /See:/ 'disassociateCertificateResponse' smart constructor.
newtype DisassociateCertificateResponse = DisassociateCertificateResponse'
  { _dcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
disassociateCertificateResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DisassociateCertificateResponse
disassociateCertificateResponse pResponseStatus_ =
  DisassociateCertificateResponse' {_dcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcrsResponseStatus :: Lens' DisassociateCertificateResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DisassociateCertificateResponse where
