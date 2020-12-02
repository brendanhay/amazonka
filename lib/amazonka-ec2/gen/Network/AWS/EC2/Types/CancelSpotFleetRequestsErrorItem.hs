{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Spot Fleet request that was not successfully canceled.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { _csfreiError ::
      !( Maybe
           CancelSpotFleetRequestsError
       ),
    _csfreiSpotFleetRequestId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelSpotFleetRequestsErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfreiError' - The error.
--
-- * 'csfreiSpotFleetRequestId' - The ID of the Spot Fleet request.
cancelSpotFleetRequestsErrorItem ::
  CancelSpotFleetRequestsErrorItem
cancelSpotFleetRequestsErrorItem =
  CancelSpotFleetRequestsErrorItem'
    { _csfreiError = Nothing,
      _csfreiSpotFleetRequestId = Nothing
    }

-- | The error.
csfreiError :: Lens' CancelSpotFleetRequestsErrorItem (Maybe CancelSpotFleetRequestsError)
csfreiError = lens _csfreiError (\s a -> s {_csfreiError = a})

-- | The ID of the Spot Fleet request.
csfreiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsErrorItem (Maybe Text)
csfreiSpotFleetRequestId = lens _csfreiSpotFleetRequestId (\s a -> s {_csfreiSpotFleetRequestId = a})

instance FromXML CancelSpotFleetRequestsErrorItem where
  parseXML x =
    CancelSpotFleetRequestsErrorItem'
      <$> (x .@? "error") <*> (x .@? "spotFleetRequestId")

instance Hashable CancelSpotFleetRequestsErrorItem

instance NFData CancelSpotFleetRequestsErrorItem
