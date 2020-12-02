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
-- Module      : Network.AWS.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Network.AWS.MediaLive.DescribeReservation
  ( -- * Creating a Request
    describeReservation,
    DescribeReservation,

    -- * Request Lenses
    drReservationId,

    -- * Destructuring the Response
    describeReservationResponse,
    DescribeReservationResponse,

    -- * Response Lenses
    drrrsState,
    drrrsResourceSpecification,
    drrrsCurrencyCode,
    drrrsARN,
    drrrsStart,
    drrrsCount,
    drrrsEnd,
    drrrsName,
    drrrsReservationId,
    drrrsOfferingId,
    drrrsRegion,
    drrrsOfferingType,
    drrrsUsagePrice,
    drrrsFixedPrice,
    drrrsDurationUnits,
    drrrsOfferingDescription,
    drrrsDuration,
    drrrsTags,
    drrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'describeReservation' smart constructor.
newtype DescribeReservation = DescribeReservation'
  { _drReservationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drReservationId' - Unique reservation ID, e.g. '1234567'
describeReservation ::
  -- | 'drReservationId'
  Text ->
  DescribeReservation
describeReservation pReservationId_ =
  DescribeReservation' {_drReservationId = pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
drReservationId :: Lens' DescribeReservation Text
drReservationId = lens _drReservationId (\s a -> s {_drReservationId = a})

instance AWSRequest DescribeReservation where
  type Rs DescribeReservation = DescribeReservationResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
            <$> (x .?> "state")
            <*> (x .?> "resourceSpecification")
            <*> (x .?> "currencyCode")
            <*> (x .?> "arn")
            <*> (x .?> "start")
            <*> (x .?> "count")
            <*> (x .?> "end")
            <*> (x .?> "name")
            <*> (x .?> "reservationId")
            <*> (x .?> "offeringId")
            <*> (x .?> "region")
            <*> (x .?> "offeringType")
            <*> (x .?> "usagePrice")
            <*> (x .?> "fixedPrice")
            <*> (x .?> "durationUnits")
            <*> (x .?> "offeringDescription")
            <*> (x .?> "duration")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReservation

instance NFData DescribeReservation

instance ToHeaders DescribeReservation where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeReservation where
  toPath DescribeReservation' {..} =
    mconcat ["/prod/reservations/", toBS _drReservationId]

instance ToQuery DescribeReservation where
  toQuery = const mempty

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'describeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
  { _drrrsState ::
      !(Maybe ReservationState),
    _drrrsResourceSpecification ::
      !( Maybe
           ReservationResourceSpecification
       ),
    _drrrsCurrencyCode :: !(Maybe Text),
    _drrrsARN :: !(Maybe Text),
    _drrrsStart :: !(Maybe Text),
    _drrrsCount :: !(Maybe Int),
    _drrrsEnd :: !(Maybe Text),
    _drrrsName :: !(Maybe Text),
    _drrrsReservationId ::
      !(Maybe Text),
    _drrrsOfferingId :: !(Maybe Text),
    _drrrsRegion :: !(Maybe Text),
    _drrrsOfferingType ::
      !(Maybe OfferingType),
    _drrrsUsagePrice :: !(Maybe Double),
    _drrrsFixedPrice :: !(Maybe Double),
    _drrrsDurationUnits ::
      !(Maybe OfferingDurationUnits),
    _drrrsOfferingDescription ::
      !(Maybe Text),
    _drrrsDuration :: !(Maybe Int),
    _drrrsTags ::
      !(Maybe (Map Text (Text))),
    _drrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrrsState' - Current state of reservation, e.g. 'ACTIVE'
--
-- * 'drrrsResourceSpecification' - Resource configuration details
--
-- * 'drrrsCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'drrrsARN' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- * 'drrrsStart' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- * 'drrrsCount' - Number of reserved resources
--
-- * 'drrrsEnd' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- * 'drrrsName' - User specified reservation name
--
-- * 'drrrsReservationId' - Unique reservation ID, e.g. '1234567'
--
-- * 'drrrsOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'drrrsRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'drrrsOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'drrrsUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'drrrsFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'drrrsDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'drrrsOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'drrrsDuration' - Lease duration, e.g. '12'
--
-- * 'drrrsTags' - A collection of key-value pairs
--
-- * 'drrrsResponseStatus' - -- | The response status code.
describeReservationResponse ::
  -- | 'drrrsResponseStatus'
  Int ->
  DescribeReservationResponse
describeReservationResponse pResponseStatus_ =
  DescribeReservationResponse'
    { _drrrsState = Nothing,
      _drrrsResourceSpecification = Nothing,
      _drrrsCurrencyCode = Nothing,
      _drrrsARN = Nothing,
      _drrrsStart = Nothing,
      _drrrsCount = Nothing,
      _drrrsEnd = Nothing,
      _drrrsName = Nothing,
      _drrrsReservationId = Nothing,
      _drrrsOfferingId = Nothing,
      _drrrsRegion = Nothing,
      _drrrsOfferingType = Nothing,
      _drrrsUsagePrice = Nothing,
      _drrrsFixedPrice = Nothing,
      _drrrsDurationUnits = Nothing,
      _drrrsOfferingDescription = Nothing,
      _drrrsDuration = Nothing,
      _drrrsTags = Nothing,
      _drrrsResponseStatus = pResponseStatus_
    }

-- | Current state of reservation, e.g. 'ACTIVE'
drrrsState :: Lens' DescribeReservationResponse (Maybe ReservationState)
drrrsState = lens _drrrsState (\s a -> s {_drrrsState = a})

-- | Resource configuration details
drrrsResourceSpecification :: Lens' DescribeReservationResponse (Maybe ReservationResourceSpecification)
drrrsResourceSpecification = lens _drrrsResourceSpecification (\s a -> s {_drrrsResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
drrrsCurrencyCode :: Lens' DescribeReservationResponse (Maybe Text)
drrrsCurrencyCode = lens _drrrsCurrencyCode (\s a -> s {_drrrsCurrencyCode = a})

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
drrrsARN :: Lens' DescribeReservationResponse (Maybe Text)
drrrsARN = lens _drrrsARN (\s a -> s {_drrrsARN = a})

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
drrrsStart :: Lens' DescribeReservationResponse (Maybe Text)
drrrsStart = lens _drrrsStart (\s a -> s {_drrrsStart = a})

-- | Number of reserved resources
drrrsCount :: Lens' DescribeReservationResponse (Maybe Int)
drrrsCount = lens _drrrsCount (\s a -> s {_drrrsCount = a})

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
drrrsEnd :: Lens' DescribeReservationResponse (Maybe Text)
drrrsEnd = lens _drrrsEnd (\s a -> s {_drrrsEnd = a})

-- | User specified reservation name
drrrsName :: Lens' DescribeReservationResponse (Maybe Text)
drrrsName = lens _drrrsName (\s a -> s {_drrrsName = a})

-- | Unique reservation ID, e.g. '1234567'
drrrsReservationId :: Lens' DescribeReservationResponse (Maybe Text)
drrrsReservationId = lens _drrrsReservationId (\s a -> s {_drrrsReservationId = a})

-- | Unique offering ID, e.g. '87654321'
drrrsOfferingId :: Lens' DescribeReservationResponse (Maybe Text)
drrrsOfferingId = lens _drrrsOfferingId (\s a -> s {_drrrsOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
drrrsRegion :: Lens' DescribeReservationResponse (Maybe Text)
drrrsRegion = lens _drrrsRegion (\s a -> s {_drrrsRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
drrrsOfferingType :: Lens' DescribeReservationResponse (Maybe OfferingType)
drrrsOfferingType = lens _drrrsOfferingType (\s a -> s {_drrrsOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
drrrsUsagePrice :: Lens' DescribeReservationResponse (Maybe Double)
drrrsUsagePrice = lens _drrrsUsagePrice (\s a -> s {_drrrsUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
drrrsFixedPrice :: Lens' DescribeReservationResponse (Maybe Double)
drrrsFixedPrice = lens _drrrsFixedPrice (\s a -> s {_drrrsFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
drrrsDurationUnits :: Lens' DescribeReservationResponse (Maybe OfferingDurationUnits)
drrrsDurationUnits = lens _drrrsDurationUnits (\s a -> s {_drrrsDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
drrrsOfferingDescription :: Lens' DescribeReservationResponse (Maybe Text)
drrrsOfferingDescription = lens _drrrsOfferingDescription (\s a -> s {_drrrsOfferingDescription = a})

-- | Lease duration, e.g. '12'
drrrsDuration :: Lens' DescribeReservationResponse (Maybe Int)
drrrsDuration = lens _drrrsDuration (\s a -> s {_drrrsDuration = a})

-- | A collection of key-value pairs
drrrsTags :: Lens' DescribeReservationResponse (HashMap Text (Text))
drrrsTags = lens _drrrsTags (\s a -> s {_drrrsTags = a}) . _Default . _Map

-- | -- | The response status code.
drrrsResponseStatus :: Lens' DescribeReservationResponse Int
drrrsResponseStatus = lens _drrrsResponseStatus (\s a -> s {_drrrsResponseStatus = a})

instance NFData DescribeReservationResponse
