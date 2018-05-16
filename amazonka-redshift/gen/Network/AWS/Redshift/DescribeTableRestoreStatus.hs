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
-- Module      : Network.AWS.Redshift.DescribeTableRestoreStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of one or more table restore requests made using the 'RestoreTableFromClusterSnapshot' API action. If you don't specify a value for the @TableRestoreRequestId@ parameter, then @DescribeTableRestoreStatus@ returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise @DescribeTableRestoreStatus@ returns the status of the table specified by @TableRestoreRequestId@ .
--
--
module Network.AWS.Redshift.DescribeTableRestoreStatus
    (
    -- * Creating a Request
      describeTableRestoreStatus
    , DescribeTableRestoreStatus
    -- * Request Lenses
    , dtrssTableRestoreRequestId
    , dtrssClusterIdentifier
    , dtrssMarker
    , dtrssMaxRecords

    -- * Destructuring the Response
    , describeTableRestoreStatusResponse
    , DescribeTableRestoreStatusResponse
    -- * Response Lenses
    , dtrsrsMarker
    , dtrsrsTableRestoreStatusDetails
    , dtrsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeTableRestoreStatus' smart constructor.
data DescribeTableRestoreStatus = DescribeTableRestoreStatus'
  { _dtrssTableRestoreRequestId :: !(Maybe Text)
  , _dtrssClusterIdentifier     :: !(Maybe Text)
  , _dtrssMarker                :: !(Maybe Text)
  , _dtrssMaxRecords            :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTableRestoreStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrssTableRestoreRequestId' - The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
--
-- * 'dtrssClusterIdentifier' - The Amazon Redshift cluster that the table is being restored to.
--
-- * 'dtrssMarker' - An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- * 'dtrssMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
describeTableRestoreStatus
    :: DescribeTableRestoreStatus
describeTableRestoreStatus =
  DescribeTableRestoreStatus'
    { _dtrssTableRestoreRequestId = Nothing
    , _dtrssClusterIdentifier = Nothing
    , _dtrssMarker = Nothing
    , _dtrssMaxRecords = Nothing
    }


-- | The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
dtrssTableRestoreRequestId :: Lens' DescribeTableRestoreStatus (Maybe Text)
dtrssTableRestoreRequestId = lens _dtrssTableRestoreRequestId (\ s a -> s{_dtrssTableRestoreRequestId = a})

-- | The Amazon Redshift cluster that the table is being restored to.
dtrssClusterIdentifier :: Lens' DescribeTableRestoreStatus (Maybe Text)
dtrssClusterIdentifier = lens _dtrssClusterIdentifier (\ s a -> s{_dtrssClusterIdentifier = a})

-- | An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
dtrssMarker :: Lens' DescribeTableRestoreStatus (Maybe Text)
dtrssMarker = lens _dtrssMarker (\ s a -> s{_dtrssMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
dtrssMaxRecords :: Lens' DescribeTableRestoreStatus (Maybe Int)
dtrssMaxRecords = lens _dtrssMaxRecords (\ s a -> s{_dtrssMaxRecords = a})

instance AWSRequest DescribeTableRestoreStatus where
        type Rs DescribeTableRestoreStatus =
             DescribeTableRestoreStatusResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "DescribeTableRestoreStatusResult"
              (\ s h x ->
                 DescribeTableRestoreStatusResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "TableRestoreStatusDetails" .!@ mempty >>=
                        may (parseXMLList "TableRestoreStatus"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTableRestoreStatus where

instance NFData DescribeTableRestoreStatus where

instance ToHeaders DescribeTableRestoreStatus where
        toHeaders = const mempty

instance ToPath DescribeTableRestoreStatus where
        toPath = const "/"

instance ToQuery DescribeTableRestoreStatus where
        toQuery DescribeTableRestoreStatus'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTableRestoreStatus" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TableRestoreRequestId" =:
                 _dtrssTableRestoreRequestId,
               "ClusterIdentifier" =: _dtrssClusterIdentifier,
               "Marker" =: _dtrssMarker,
               "MaxRecords" =: _dtrssMaxRecords]

-- |
--
--
--
-- /See:/ 'describeTableRestoreStatusResponse' smart constructor.
data DescribeTableRestoreStatusResponse = DescribeTableRestoreStatusResponse'
  { _dtrsrsMarker                    :: !(Maybe Text)
  , _dtrsrsTableRestoreStatusDetails :: !(Maybe [TableRestoreStatus])
  , _dtrsrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTableRestoreStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsrsMarker' - A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
--
-- * 'dtrsrsTableRestoreStatusDetails' - A list of status details for one or more table restore requests.
--
-- * 'dtrsrsResponseStatus' - -- | The response status code.
describeTableRestoreStatusResponse
    :: Int -- ^ 'dtrsrsResponseStatus'
    -> DescribeTableRestoreStatusResponse
describeTableRestoreStatusResponse pResponseStatus_ =
  DescribeTableRestoreStatusResponse'
    { _dtrsrsMarker = Nothing
    , _dtrsrsTableRestoreStatusDetails = Nothing
    , _dtrsrsResponseStatus = pResponseStatus_
    }


-- | A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
dtrsrsMarker :: Lens' DescribeTableRestoreStatusResponse (Maybe Text)
dtrsrsMarker = lens _dtrsrsMarker (\ s a -> s{_dtrsrsMarker = a})

-- | A list of status details for one or more table restore requests.
dtrsrsTableRestoreStatusDetails :: Lens' DescribeTableRestoreStatusResponse [TableRestoreStatus]
dtrsrsTableRestoreStatusDetails = lens _dtrsrsTableRestoreStatusDetails (\ s a -> s{_dtrsrsTableRestoreStatusDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsrsResponseStatus :: Lens' DescribeTableRestoreStatusResponse Int
dtrsrsResponseStatus = lens _dtrsrsResponseStatus (\ s a -> s{_dtrsrsResponseStatus = a})

instance NFData DescribeTableRestoreStatusResponse
         where
