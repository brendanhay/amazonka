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
-- Module      : Network.AWS.MediaLive.PurchaseOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase an offering and create a reservation.
module Network.AWS.MediaLive.PurchaseOffering
  ( -- * Creating a Request
    purchaseOffering,
    PurchaseOffering,

    -- * Request Lenses
    poRequestId,
    poStart,
    poName,
    poTags,
    poOfferingId,
    poCount,

    -- * Destructuring the Response
    purchaseOfferingResponse,
    PurchaseOfferingResponse,

    -- * Response Lenses
    porsReservation,
    porsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for PurchaseOfferingRequest
--
-- /See:/ 'purchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { _poRequestId ::
      !(Maybe Text),
    _poStart :: !(Maybe Text),
    _poName :: !(Maybe Text),
    _poTags :: !(Maybe (Map Text (Text))),
    _poOfferingId :: !Text,
    _poCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PurchaseOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poRequestId' - Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
--
-- * 'poStart' - Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
--
-- * 'poName' - Name for the new reservation
--
-- * 'poTags' - A collection of key-value pairs
--
-- * 'poOfferingId' - Offering to purchase, e.g. '87654321'
--
-- * 'poCount' - Number of resources
purchaseOffering ::
  -- | 'poOfferingId'
  Text ->
  -- | 'poCount'
  Natural ->
  PurchaseOffering
purchaseOffering pOfferingId_ pCount_ =
  PurchaseOffering'
    { _poRequestId = Nothing,
      _poStart = Nothing,
      _poName = Nothing,
      _poTags = Nothing,
      _poOfferingId = pOfferingId_,
      _poCount = _Nat # pCount_
    }

-- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
poRequestId :: Lens' PurchaseOffering (Maybe Text)
poRequestId = lens _poRequestId (\s a -> s {_poRequestId = a})

-- | Requested reservation start time (UTC) in ISO-8601 format. The specified time must be between the first day of the current month and one year from now. If no value is given, the default is now.
poStart :: Lens' PurchaseOffering (Maybe Text)
poStart = lens _poStart (\s a -> s {_poStart = a})

-- | Name for the new reservation
poName :: Lens' PurchaseOffering (Maybe Text)
poName = lens _poName (\s a -> s {_poName = a})

-- | A collection of key-value pairs
poTags :: Lens' PurchaseOffering (HashMap Text (Text))
poTags = lens _poTags (\s a -> s {_poTags = a}) . _Default . _Map

-- | Offering to purchase, e.g. '87654321'
poOfferingId :: Lens' PurchaseOffering Text
poOfferingId = lens _poOfferingId (\s a -> s {_poOfferingId = a})

-- | Number of resources
poCount :: Lens' PurchaseOffering Natural
poCount = lens _poCount (\s a -> s {_poCount = a}) . _Nat

instance AWSRequest PurchaseOffering where
  type Rs PurchaseOffering = PurchaseOfferingResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            <$> (x .?> "reservation") <*> (pure (fromEnum s))
      )

instance Hashable PurchaseOffering

instance NFData PurchaseOffering

instance ToHeaders PurchaseOffering where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PurchaseOffering where
  toJSON PurchaseOffering' {..} =
    object
      ( catMaybes
          [ ("requestId" .=) <$> _poRequestId,
            ("start" .=) <$> _poStart,
            ("name" .=) <$> _poName,
            ("tags" .=) <$> _poTags,
            Just ("count" .= _poCount)
          ]
      )

instance ToPath PurchaseOffering where
  toPath PurchaseOffering' {..} =
    mconcat ["/prod/offerings/", toBS _poOfferingId, "/purchase"]

instance ToQuery PurchaseOffering where
  toQuery = const mempty

-- | Placeholder documentation for PurchaseOfferingResponse
--
-- /See:/ 'purchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { _porsReservation ::
      !(Maybe Reservation),
    _porsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PurchaseOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porsReservation' - Undocumented member.
--
-- * 'porsResponseStatus' - -- | The response status code.
purchaseOfferingResponse ::
  -- | 'porsResponseStatus'
  Int ->
  PurchaseOfferingResponse
purchaseOfferingResponse pResponseStatus_ =
  PurchaseOfferingResponse'
    { _porsReservation = Nothing,
      _porsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
porsReservation :: Lens' PurchaseOfferingResponse (Maybe Reservation)
porsReservation = lens _porsReservation (\s a -> s {_porsReservation = a})

-- | -- | The response status code.
porsResponseStatus :: Lens' PurchaseOfferingResponse Int
porsResponseStatus = lens _porsResponseStatus (\s a -> s {_porsResponseStatus = a})

instance NFData PurchaseOfferingResponse
