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
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
--
--
module Network.AWS.IoT.DescribeCACertificate
    (
    -- * Creating a Request
      describeCACertificate
    , DescribeCACertificate
    -- * Request Lenses
    , dCertificateId

    -- * Destructuring the Response
    , describeCACertificateResponse
    , DescribeCACertificateResponse
    -- * Response Lenses
    , desrsCertificateDescription
    , desrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DescribeCACertificate operation.
--
--
--
-- /See:/ 'describeCACertificate' smart constructor.
newtype DescribeCACertificate = DescribeCACertificate'
  { _dCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateId' - The CA certificate identifier.
describeCACertificate
    :: Text -- ^ 'dCertificateId'
    -> DescribeCACertificate
describeCACertificate pCertificateId_ =
  DescribeCACertificate' {_dCertificateId = pCertificateId_}


-- | The CA certificate identifier.
dCertificateId :: Lens' DescribeCACertificate Text
dCertificateId = lens _dCertificateId (\ s a -> s{_dCertificateId = a});

instance AWSRequest DescribeCACertificate where
        type Rs DescribeCACertificate =
             DescribeCACertificateResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCACertificateResponse' <$>
                   (x .?> "certificateDescription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeCACertificate where

instance NFData DescribeCACertificate where

instance ToHeaders DescribeCACertificate where
        toHeaders = const mempty

instance ToPath DescribeCACertificate where
        toPath DescribeCACertificate'{..}
          = mconcat ["/cacertificate/", toBS _dCertificateId]

instance ToQuery DescribeCACertificate where
        toQuery = const mempty

-- | The output from the DescribeCACertificate operation.
--
--
--
-- /See:/ 'describeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'
  { _desrsCertificateDescription :: !(Maybe CACertificateDescription)
  , _desrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCACertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsCertificateDescription' - The CA certificate description.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeCACertificateResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeCACertificateResponse
describeCACertificateResponse pResponseStatus_ =
  DescribeCACertificateResponse'
  { _desrsCertificateDescription = Nothing
  , _desrsResponseStatus = pResponseStatus_
  }


-- | The CA certificate description.
desrsCertificateDescription :: Lens' DescribeCACertificateResponse (Maybe CACertificateDescription)
desrsCertificateDescription = lens _desrsCertificateDescription (\ s a -> s{_desrsCertificateDescription = a});

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeCACertificateResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});

instance NFData DescribeCACertificateResponse where
