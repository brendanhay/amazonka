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
-- Module      : Network.AWS.MediaLive.DeleteReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an expired reservation.
module Network.AWS.MediaLive.DeleteReservation
  ( -- * Creating a Request
    deleteReservation,
    DeleteReservation,

    -- * Request Lenses
    dReservationId,

    -- * Destructuring the Response
    deleteReservationResponse,
    DeleteReservationResponse,

    -- * Response Lenses
    drrsState,
    drrsResourceSpecification,
    drrsCurrencyCode,
    drrsARN,
    drrsStart,
    drrsCount,
    drrsEnd,
    drrsName,
    drrsReservationId,
    drrsOfferingId,
    drrsRegion,
    drrsOfferingType,
    drrsUsagePrice,
    drrsFixedPrice,
    drrsDurationUnits,
    drrsOfferingDescription,
    drrsDuration,
    drrsTags,
    drrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteReservationRequest
--
-- /See:/ 'deleteReservation' smart constructor.
newtype DeleteReservation = DeleteReservation'
  { _dReservationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dReservationId' - Unique reservation ID, e.g. '1234567'
deleteReservation ::
  -- | 'dReservationId'
  Text ->
  DeleteReservation
deleteReservation pReservationId_ =
  DeleteReservation' {_dReservationId = pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
dReservationId :: Lens' DeleteReservation Text
dReservationId = lens _dReservationId (\s a -> s {_dReservationId = a})

instance AWSRequest DeleteReservation where
  type Rs DeleteReservation = DeleteReservationResponse
  request = delete mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DeleteReservationResponse'
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

instance Hashable DeleteReservation

instance NFData DeleteReservation

instance ToHeaders DeleteReservation where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteReservation where
  toPath DeleteReservation' {..} =
    mconcat ["/prod/reservations/", toBS _dReservationId]

instance ToQuery DeleteReservation where
  toQuery = const mempty

-- | Placeholder documentation for DeleteReservationResponse
--
-- /See:/ 'deleteReservationResponse' smart constructor.
data DeleteReservationResponse = DeleteReservationResponse'
  { _drrsState ::
      !(Maybe ReservationState),
    _drrsResourceSpecification ::
      !( Maybe
           ReservationResourceSpecification
       ),
    _drrsCurrencyCode :: !(Maybe Text),
    _drrsARN :: !(Maybe Text),
    _drrsStart :: !(Maybe Text),
    _drrsCount :: !(Maybe Int),
    _drrsEnd :: !(Maybe Text),
    _drrsName :: !(Maybe Text),
    _drrsReservationId :: !(Maybe Text),
    _drrsOfferingId :: !(Maybe Text),
    _drrsRegion :: !(Maybe Text),
    _drrsOfferingType ::
      !(Maybe OfferingType),
    _drrsUsagePrice :: !(Maybe Double),
    _drrsFixedPrice :: !(Maybe Double),
    _drrsDurationUnits ::
      !(Maybe OfferingDurationUnits),
    _drrsOfferingDescription ::
      !(Maybe Text),
    _drrsDuration :: !(Maybe Int),
    _drrsTags :: !(Maybe (Map Text (Text))),
    _drrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsState' - Current state of reservation, e.g. 'ACTIVE'
--
-- * 'drrsResourceSpecification' - Resource configuration details
--
-- * 'drrsCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'drrsARN' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- * 'drrsStart' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- * 'drrsCount' - Number of reserved resources
--
-- * 'drrsEnd' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- * 'drrsName' - User specified reservation name
--
-- * 'drrsReservationId' - Unique reservation ID, e.g. '1234567'
--
-- * 'drrsOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'drrsRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'drrsOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'drrsUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'drrsFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'drrsDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'drrsOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'drrsDuration' - Lease duration, e.g. '12'
--
-- * 'drrsTags' - A collection of key-value pairs
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteReservationResponse ::
  -- | 'drrsResponseStatus'
  Int ->
  DeleteReservationResponse
deleteReservationResponse pResponseStatus_ =
  DeleteReservationResponse'
    { _drrsState = Nothing,
      _drrsResourceSpecification = Nothing,
      _drrsCurrencyCode = Nothing,
      _drrsARN = Nothing,
      _drrsStart = Nothing,
      _drrsCount = Nothing,
      _drrsEnd = Nothing,
      _drrsName = Nothing,
      _drrsReservationId = Nothing,
      _drrsOfferingId = Nothing,
      _drrsRegion = Nothing,
      _drrsOfferingType = Nothing,
      _drrsUsagePrice = Nothing,
      _drrsFixedPrice = Nothing,
      _drrsDurationUnits = Nothing,
      _drrsOfferingDescription = Nothing,
      _drrsDuration = Nothing,
      _drrsTags = Nothing,
      _drrsResponseStatus = pResponseStatus_
    }

-- | Current state of reservation, e.g. 'ACTIVE'
drrsState :: Lens' DeleteReservationResponse (Maybe ReservationState)
drrsState = lens _drrsState (\s a -> s {_drrsState = a})

-- | Resource configuration details
drrsResourceSpecification :: Lens' DeleteReservationResponse (Maybe ReservationResourceSpecification)
drrsResourceSpecification = lens _drrsResourceSpecification (\s a -> s {_drrsResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
drrsCurrencyCode :: Lens' DeleteReservationResponse (Maybe Text)
drrsCurrencyCode = lens _drrsCurrencyCode (\s a -> s {_drrsCurrencyCode = a})

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
drrsARN :: Lens' DeleteReservationResponse (Maybe Text)
drrsARN = lens _drrsARN (\s a -> s {_drrsARN = a})

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
drrsStart :: Lens' DeleteReservationResponse (Maybe Text)
drrsStart = lens _drrsStart (\s a -> s {_drrsStart = a})

-- | Number of reserved resources
drrsCount :: Lens' DeleteReservationResponse (Maybe Int)
drrsCount = lens _drrsCount (\s a -> s {_drrsCount = a})

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
drrsEnd :: Lens' DeleteReservationResponse (Maybe Text)
drrsEnd = lens _drrsEnd (\s a -> s {_drrsEnd = a})

-- | User specified reservation name
drrsName :: Lens' DeleteReservationResponse (Maybe Text)
drrsName = lens _drrsName (\s a -> s {_drrsName = a})

-- | Unique reservation ID, e.g. '1234567'
drrsReservationId :: Lens' DeleteReservationResponse (Maybe Text)
drrsReservationId = lens _drrsReservationId (\s a -> s {_drrsReservationId = a})

-- | Unique offering ID, e.g. '87654321'
drrsOfferingId :: Lens' DeleteReservationResponse (Maybe Text)
drrsOfferingId = lens _drrsOfferingId (\s a -> s {_drrsOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
drrsRegion :: Lens' DeleteReservationResponse (Maybe Text)
drrsRegion = lens _drrsRegion (\s a -> s {_drrsRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
drrsOfferingType :: Lens' DeleteReservationResponse (Maybe OfferingType)
drrsOfferingType = lens _drrsOfferingType (\s a -> s {_drrsOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
drrsUsagePrice :: Lens' DeleteReservationResponse (Maybe Double)
drrsUsagePrice = lens _drrsUsagePrice (\s a -> s {_drrsUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
drrsFixedPrice :: Lens' DeleteReservationResponse (Maybe Double)
drrsFixedPrice = lens _drrsFixedPrice (\s a -> s {_drrsFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
drrsDurationUnits :: Lens' DeleteReservationResponse (Maybe OfferingDurationUnits)
drrsDurationUnits = lens _drrsDurationUnits (\s a -> s {_drrsDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
drrsOfferingDescription :: Lens' DeleteReservationResponse (Maybe Text)
drrsOfferingDescription = lens _drrsOfferingDescription (\s a -> s {_drrsOfferingDescription = a})

-- | Lease duration, e.g. '12'
drrsDuration :: Lens' DeleteReservationResponse (Maybe Int)
drrsDuration = lens _drrsDuration (\s a -> s {_drrsDuration = a})

-- | A collection of key-value pairs
drrsTags :: Lens' DeleteReservationResponse (HashMap Text (Text))
drrsTags = lens _drrsTags (\s a -> s {_drrsTags = a}) . _Default . _Map

-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteReservationResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\s a -> s {_drrsResponseStatus = a})

instance NFData DeleteReservationResponse
