{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BatchState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Spot Fleet request that was successfully canceled.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { _csfrsiCurrentSpotFleetRequestState ::
      !(Maybe BatchState),
    _csfrsiSpotFleetRequestId ::
      !(Maybe Text),
    _csfrsiPreviousSpotFleetRequestState ::
      !(Maybe BatchState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelSpotFleetRequestsSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfrsiCurrentSpotFleetRequestState' - The current state of the Spot Fleet request.
--
-- * 'csfrsiSpotFleetRequestId' - The ID of the Spot Fleet request.
--
-- * 'csfrsiPreviousSpotFleetRequestState' - The previous state of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem ::
  CancelSpotFleetRequestsSuccessItem
cancelSpotFleetRequestsSuccessItem =
  CancelSpotFleetRequestsSuccessItem'
    { _csfrsiCurrentSpotFleetRequestState =
        Nothing,
      _csfrsiSpotFleetRequestId = Nothing,
      _csfrsiPreviousSpotFleetRequestState = Nothing
    }

-- | The current state of the Spot Fleet request.
csfrsiCurrentSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem (Maybe BatchState)
csfrsiCurrentSpotFleetRequestState = lens _csfrsiCurrentSpotFleetRequestState (\s a -> s {_csfrsiCurrentSpotFleetRequestState = a})

-- | The ID of the Spot Fleet request.
csfrsiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsSuccessItem (Maybe Text)
csfrsiSpotFleetRequestId = lens _csfrsiSpotFleetRequestId (\s a -> s {_csfrsiSpotFleetRequestId = a})

-- | The previous state of the Spot Fleet request.
csfrsiPreviousSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem (Maybe BatchState)
csfrsiPreviousSpotFleetRequestState = lens _csfrsiPreviousSpotFleetRequestState (\s a -> s {_csfrsiPreviousSpotFleetRequestState = a})

instance FromXML CancelSpotFleetRequestsSuccessItem where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      <$> (x .@? "currentSpotFleetRequestState")
      <*> (x .@? "spotFleetRequestId")
      <*> (x .@? "previousSpotFleetRequestState")

instance Hashable CancelSpotFleetRequestsSuccessItem

instance NFData CancelSpotFleetRequestsSuccessItem
