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
-- Module      : Network.AWS.MediaLive.DescribeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for an offering.
module Network.AWS.MediaLive.DescribeOffering
  ( -- * Creating a Request
    describeOffering,
    DescribeOffering,

    -- * Request Lenses
    doOfferingId,

    -- * Destructuring the Response
    describeOfferingResponse,
    DescribeOfferingResponse,

    -- * Response Lenses
    dorsResourceSpecification,
    dorsCurrencyCode,
    dorsARN,
    dorsOfferingId,
    dorsRegion,
    dorsOfferingType,
    dorsUsagePrice,
    dorsFixedPrice,
    dorsDurationUnits,
    dorsOfferingDescription,
    dorsDuration,
    dorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeOfferingRequest
--
-- /See:/ 'describeOffering' smart constructor.
newtype DescribeOffering = DescribeOffering' {_doOfferingId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doOfferingId' - Unique offering ID, e.g. '87654321'
describeOffering ::
  -- | 'doOfferingId'
  Text ->
  DescribeOffering
describeOffering pOfferingId_ =
  DescribeOffering' {_doOfferingId = pOfferingId_}

-- | Unique offering ID, e.g. '87654321'
doOfferingId :: Lens' DescribeOffering Text
doOfferingId = lens _doOfferingId (\s a -> s {_doOfferingId = a})

instance AWSRequest DescribeOffering where
  type Rs DescribeOffering = DescribeOfferingResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeOfferingResponse'
            <$> (x .?> "resourceSpecification")
            <*> (x .?> "currencyCode")
            <*> (x .?> "arn")
            <*> (x .?> "offeringId")
            <*> (x .?> "region")
            <*> (x .?> "offeringType")
            <*> (x .?> "usagePrice")
            <*> (x .?> "fixedPrice")
            <*> (x .?> "durationUnits")
            <*> (x .?> "offeringDescription")
            <*> (x .?> "duration")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOffering

instance NFData DescribeOffering

instance ToHeaders DescribeOffering where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeOffering where
  toPath DescribeOffering' {..} =
    mconcat ["/prod/offerings/", toBS _doOfferingId]

instance ToQuery DescribeOffering where
  toQuery = const mempty

-- | Placeholder documentation for DescribeOfferingResponse
--
-- /See:/ 'describeOfferingResponse' smart constructor.
data DescribeOfferingResponse = DescribeOfferingResponse'
  { _dorsResourceSpecification ::
      !(Maybe ReservationResourceSpecification),
    _dorsCurrencyCode :: !(Maybe Text),
    _dorsARN :: !(Maybe Text),
    _dorsOfferingId :: !(Maybe Text),
    _dorsRegion :: !(Maybe Text),
    _dorsOfferingType ::
      !(Maybe OfferingType),
    _dorsUsagePrice :: !(Maybe Double),
    _dorsFixedPrice :: !(Maybe Double),
    _dorsDurationUnits ::
      !(Maybe OfferingDurationUnits),
    _dorsOfferingDescription :: !(Maybe Text),
    _dorsDuration :: !(Maybe Int),
    _dorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsResourceSpecification' - Resource configuration details
--
-- * 'dorsCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'dorsARN' - Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- * 'dorsOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'dorsRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'dorsOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'dorsUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'dorsFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'dorsDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'dorsOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'dorsDuration' - Lease duration, e.g. '12'
--
-- * 'dorsResponseStatus' - -- | The response status code.
describeOfferingResponse ::
  -- | 'dorsResponseStatus'
  Int ->
  DescribeOfferingResponse
describeOfferingResponse pResponseStatus_ =
  DescribeOfferingResponse'
    { _dorsResourceSpecification = Nothing,
      _dorsCurrencyCode = Nothing,
      _dorsARN = Nothing,
      _dorsOfferingId = Nothing,
      _dorsRegion = Nothing,
      _dorsOfferingType = Nothing,
      _dorsUsagePrice = Nothing,
      _dorsFixedPrice = Nothing,
      _dorsDurationUnits = Nothing,
      _dorsOfferingDescription = Nothing,
      _dorsDuration = Nothing,
      _dorsResponseStatus = pResponseStatus_
    }

-- | Resource configuration details
dorsResourceSpecification :: Lens' DescribeOfferingResponse (Maybe ReservationResourceSpecification)
dorsResourceSpecification = lens _dorsResourceSpecification (\s a -> s {_dorsResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
dorsCurrencyCode :: Lens' DescribeOfferingResponse (Maybe Text)
dorsCurrencyCode = lens _dorsCurrencyCode (\s a -> s {_dorsCurrencyCode = a})

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
dorsARN :: Lens' DescribeOfferingResponse (Maybe Text)
dorsARN = lens _dorsARN (\s a -> s {_dorsARN = a})

-- | Unique offering ID, e.g. '87654321'
dorsOfferingId :: Lens' DescribeOfferingResponse (Maybe Text)
dorsOfferingId = lens _dorsOfferingId (\s a -> s {_dorsOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
dorsRegion :: Lens' DescribeOfferingResponse (Maybe Text)
dorsRegion = lens _dorsRegion (\s a -> s {_dorsRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
dorsOfferingType :: Lens' DescribeOfferingResponse (Maybe OfferingType)
dorsOfferingType = lens _dorsOfferingType (\s a -> s {_dorsOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
dorsUsagePrice :: Lens' DescribeOfferingResponse (Maybe Double)
dorsUsagePrice = lens _dorsUsagePrice (\s a -> s {_dorsUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
dorsFixedPrice :: Lens' DescribeOfferingResponse (Maybe Double)
dorsFixedPrice = lens _dorsFixedPrice (\s a -> s {_dorsFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
dorsDurationUnits :: Lens' DescribeOfferingResponse (Maybe OfferingDurationUnits)
dorsDurationUnits = lens _dorsDurationUnits (\s a -> s {_dorsDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
dorsOfferingDescription :: Lens' DescribeOfferingResponse (Maybe Text)
dorsOfferingDescription = lens _dorsOfferingDescription (\s a -> s {_dorsOfferingDescription = a})

-- | Lease duration, e.g. '12'
dorsDuration :: Lens' DescribeOfferingResponse (Maybe Int)
dorsDuration = lens _dorsDuration (\s a -> s {_dorsDuration = a})

-- | -- | The response status code.
dorsResponseStatus :: Lens' DescribeOfferingResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\s a -> s {_dorsResponseStatus = a})

instance NFData DescribeOfferingResponse
