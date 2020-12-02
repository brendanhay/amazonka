{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelBatchErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Spot Fleet error.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { _csfreCode ::
      !(Maybe CancelBatchErrorCode),
    _csfreMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelSpotFleetRequestsError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfreCode' - The error code.
--
-- * 'csfreMessage' - The description for the error code.
cancelSpotFleetRequestsError ::
  CancelSpotFleetRequestsError
cancelSpotFleetRequestsError =
  CancelSpotFleetRequestsError'
    { _csfreCode = Nothing,
      _csfreMessage = Nothing
    }

-- | The error code.
csfreCode :: Lens' CancelSpotFleetRequestsError (Maybe CancelBatchErrorCode)
csfreCode = lens _csfreCode (\s a -> s {_csfreCode = a})

-- | The description for the error code.
csfreMessage :: Lens' CancelSpotFleetRequestsError (Maybe Text)
csfreMessage = lens _csfreMessage (\s a -> s {_csfreMessage = a})

instance FromXML CancelSpotFleetRequestsError where
  parseXML x =
    CancelSpotFleetRequestsError'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable CancelSpotFleetRequestsError

instance NFData CancelSpotFleetRequestsError
