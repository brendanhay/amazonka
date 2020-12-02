{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption algorithm for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1EncryptionAlgorithmsListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsListValue = Phase1EncryptionAlgorithmsListValue'
  { _pealveValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase1EncryptionAlgorithmsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pealveValue' - The value for the encryption algorithm.
phase1EncryptionAlgorithmsListValue ::
  Phase1EncryptionAlgorithmsListValue
phase1EncryptionAlgorithmsListValue =
  Phase1EncryptionAlgorithmsListValue' {_pealveValue = Nothing}

-- | The value for the encryption algorithm.
pealveValue :: Lens' Phase1EncryptionAlgorithmsListValue (Maybe Text)
pealveValue = lens _pealveValue (\s a -> s {_pealveValue = a})

instance FromXML Phase1EncryptionAlgorithmsListValue where
  parseXML x =
    Phase1EncryptionAlgorithmsListValue' <$> (x .@? "value")

instance Hashable Phase1EncryptionAlgorithmsListValue

instance NFData Phase1EncryptionAlgorithmsListValue
