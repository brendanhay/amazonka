{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.SingleMasterConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
--
--
--
-- /See:/ 'singleMasterConfiguration' smart constructor.
newtype SingleMasterConfiguration = SingleMasterConfiguration'
  { _smcMessageTtlSeconds ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SingleMasterConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smcMessageTtlSeconds' - The period of time a signaling channel retains underlivered messages before they are discarded.
singleMasterConfiguration ::
  SingleMasterConfiguration
singleMasterConfiguration =
  SingleMasterConfiguration' {_smcMessageTtlSeconds = Nothing}

-- | The period of time a signaling channel retains underlivered messages before they are discarded.
smcMessageTtlSeconds :: Lens' SingleMasterConfiguration (Maybe Natural)
smcMessageTtlSeconds = lens _smcMessageTtlSeconds (\s a -> s {_smcMessageTtlSeconds = a}) . mapping _Nat

instance FromJSON SingleMasterConfiguration where
  parseJSON =
    withObject
      "SingleMasterConfiguration"
      (\x -> SingleMasterConfiguration' <$> (x .:? "MessageTtlSeconds"))

instance Hashable SingleMasterConfiguration

instance NFData SingleMasterConfiguration

instance ToJSON SingleMasterConfiguration where
  toJSON SingleMasterConfiguration' {..} =
    object
      (catMaybes [("MessageTtlSeconds" .=) <$> _smcMessageTtlSeconds])
