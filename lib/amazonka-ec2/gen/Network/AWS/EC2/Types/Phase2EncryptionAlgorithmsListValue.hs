{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption algorithm for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2EncryptionAlgorithmsListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsListValue = Phase2EncryptionAlgorithmsListValue'
  { _pealvValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase2EncryptionAlgorithmsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pealvValue' - The encryption algorithm.
phase2EncryptionAlgorithmsListValue ::
  Phase2EncryptionAlgorithmsListValue
phase2EncryptionAlgorithmsListValue =
  Phase2EncryptionAlgorithmsListValue' {_pealvValue = Nothing}

-- | The encryption algorithm.
pealvValue :: Lens' Phase2EncryptionAlgorithmsListValue (Maybe Text)
pealvValue = lens _pealvValue (\s a -> s {_pealvValue = a})

instance FromXML Phase2EncryptionAlgorithmsListValue where
  parseXML x =
    Phase2EncryptionAlgorithmsListValue' <$> (x .@? "value")

instance Hashable Phase2EncryptionAlgorithmsListValue

instance NFData Phase2EncryptionAlgorithmsListValue
