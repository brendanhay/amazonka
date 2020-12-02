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
-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this AWS account.
--
--
module Network.AWS.RDS.DescribeCertificates
    (
    -- * Creating a Request
      describeCertificates
    , DescribeCertificates
    -- * Request Lenses
    , dcFilters
    , dcCertificateIdentifier
    , dcMarker
    , dcMaxRecords

    -- * Destructuring the Response
    , describeCertificatesResponse
    , DescribeCertificatesResponse
    -- * Response Lenses
    , dcrsCertificates
    , dcrsMarker
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'
  { _dcFilters               :: !(Maybe [Filter])
  , _dcCertificateIdentifier :: !(Maybe Text)
  , _dcMarker                :: !(Maybe Text)
  , _dcMaxRecords            :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcFilters' - This parameter is not currently supported.
--
-- * 'dcCertificateIdentifier' - The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive. Constraints:     * Must match an existing CertificateIdentifier.
--
-- * 'dcMarker' - An optional pagination token provided by a previous 'DescribeCertificates' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeCertificates
    :: DescribeCertificates
describeCertificates =
  DescribeCertificates'
    { _dcFilters = Nothing
    , _dcCertificateIdentifier = Nothing
    , _dcMarker = Nothing
    , _dcMaxRecords = Nothing
    }


-- | This parameter is not currently supported.
dcFilters :: Lens' DescribeCertificates [Filter]
dcFilters = lens _dcFilters (\ s a -> s{_dcFilters = a}) . _Default . _Coerce

-- | The user-supplied certificate identifier. If this parameter is specified, information for only the identified certificate is returned. This parameter isn't case-sensitive. Constraints:     * Must match an existing CertificateIdentifier.
dcCertificateIdentifier :: Lens' DescribeCertificates (Maybe Text)
dcCertificateIdentifier = lens _dcCertificateIdentifier (\ s a -> s{_dcCertificateIdentifier = a})

-- | An optional pagination token provided by a previous 'DescribeCertificates' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcMarker :: Lens' DescribeCertificates (Maybe Text)
dcMarker = lens _dcMarker (\ s a -> s{_dcMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dcMaxRecords :: Lens' DescribeCertificates (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\ s a -> s{_dcMaxRecords = a})

instance AWSRequest DescribeCertificates where
        type Rs DescribeCertificates =
             DescribeCertificatesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeCertificatesResult"
              (\ s h x ->
                 DescribeCertificatesResponse' <$>
                   (x .@? "Certificates" .!@ mempty >>=
                      may (parseXMLList "Certificate"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCertificates where

instance NFData DescribeCertificates where

instance ToHeaders DescribeCertificates where
        toHeaders = const mempty

instance ToPath DescribeCertificates where
        toPath = const "/"

instance ToQuery DescribeCertificates where
        toQuery DescribeCertificates'{..}
          = mconcat
              ["Action" =: ("DescribeCertificates" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dcFilters),
               "CertificateIdentifier" =: _dcCertificateIdentifier,
               "Marker" =: _dcMarker, "MaxRecords" =: _dcMaxRecords]

-- | Data returned by the __DescribeCertificates__ action.
--
--
--
-- /See:/ 'describeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'
  { _dcrsCertificates   :: !(Maybe [Certificate])
  , _dcrsMarker         :: !(Maybe Text)
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCertificates' - The list of 'Certificate' objects for the AWS account.
--
-- * 'dcrsMarker' - An optional pagination token provided by a previous 'DescribeCertificates' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCertificatesResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCertificatesResponse
describeCertificatesResponse pResponseStatus_ =
  DescribeCertificatesResponse'
    { _dcrsCertificates = Nothing
    , _dcrsMarker = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | The list of 'Certificate' objects for the AWS account.
dcrsCertificates :: Lens' DescribeCertificatesResponse [Certificate]
dcrsCertificates = lens _dcrsCertificates (\ s a -> s{_dcrsCertificates = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous 'DescribeCertificates' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcrsMarker :: Lens' DescribeCertificatesResponse (Maybe Text)
dcrsMarker = lens _dcrsMarker (\ s a -> s{_dcrsMarker = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCertificatesResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeCertificatesResponse where
