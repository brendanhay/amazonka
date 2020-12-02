{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the encryption algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsRequestListValue = Phase1EncryptionAlgorithmsRequestListValue'
  { _pearlvValue ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'Phase1EncryptionAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pearlvValue' - The value for the encryption algorithm.
phase1EncryptionAlgorithmsRequestListValue ::
  Phase1EncryptionAlgorithmsRequestListValue
phase1EncryptionAlgorithmsRequestListValue =
  Phase1EncryptionAlgorithmsRequestListValue'
    { _pearlvValue =
        Nothing
    }

-- | The value for the encryption algorithm.
pearlvValue :: Lens' Phase1EncryptionAlgorithmsRequestListValue (Maybe Text)
pearlvValue = lens _pearlvValue (\s a -> s {_pearlvValue = a})

instance Hashable Phase1EncryptionAlgorithmsRequestListValue

instance NFData Phase1EncryptionAlgorithmsRequestListValue

instance ToQuery Phase1EncryptionAlgorithmsRequestListValue where
  toQuery Phase1EncryptionAlgorithmsRequestListValue' {..} =
    mconcat ["Value" =: _pearlvValue]
