{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelledSpotInstanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelledSpotInstanceRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelSpotInstanceRequestState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a request to cancel a Spot Instance.
--
--
--
-- /See:/ 'cancelledSpotInstanceRequest' smart constructor.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'
  { _csirState ::
      !( Maybe
           CancelSpotInstanceRequestState
       ),
    _csirSpotInstanceRequestId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelledSpotInstanceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirState' - The state of the Spot Instance request.
--
-- * 'csirSpotInstanceRequestId' - The ID of the Spot Instance request.
cancelledSpotInstanceRequest ::
  CancelledSpotInstanceRequest
cancelledSpotInstanceRequest =
  CancelledSpotInstanceRequest'
    { _csirState = Nothing,
      _csirSpotInstanceRequestId = Nothing
    }

-- | The state of the Spot Instance request.
csirState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csirState = lens _csirState (\s a -> s {_csirState = a})

-- | The ID of the Spot Instance request.
csirSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csirSpotInstanceRequestId = lens _csirSpotInstanceRequestId (\s a -> s {_csirSpotInstanceRequestId = a})

instance FromXML CancelledSpotInstanceRequest where
  parseXML x =
    CancelledSpotInstanceRequest'
      <$> (x .@? "state") <*> (x .@? "spotInstanceRequestId")

instance Hashable CancelledSpotInstanceRequest

instance NFData CancelledSpotInstanceRequest
