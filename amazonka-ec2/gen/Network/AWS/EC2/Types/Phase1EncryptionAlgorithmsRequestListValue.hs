{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the encryption algorithm for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- /See:/ 'newPhase1EncryptionAlgorithmsRequestListValue' smart constructor.
data Phase1EncryptionAlgorithmsRequestListValue = Phase1EncryptionAlgorithmsRequestListValue'
  { -- | The value for the encryption algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase1EncryptionAlgorithmsRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1EncryptionAlgorithmsRequestListValue_value' - The value for the encryption algorithm.
newPhase1EncryptionAlgorithmsRequestListValue ::
  Phase1EncryptionAlgorithmsRequestListValue
newPhase1EncryptionAlgorithmsRequestListValue =
  Phase1EncryptionAlgorithmsRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The value for the encryption algorithm.
phase1EncryptionAlgorithmsRequestListValue_value :: Lens.Lens' Phase1EncryptionAlgorithmsRequestListValue (Prelude.Maybe Prelude.Text)
phase1EncryptionAlgorithmsRequestListValue_value = Lens.lens (\Phase1EncryptionAlgorithmsRequestListValue' {value} -> value) (\s@Phase1EncryptionAlgorithmsRequestListValue' {} a -> s {value = a} :: Phase1EncryptionAlgorithmsRequestListValue)

instance
  Prelude.Hashable
    Phase1EncryptionAlgorithmsRequestListValue

instance
  Prelude.NFData
    Phase1EncryptionAlgorithmsRequestListValue

instance
  Prelude.ToQuery
    Phase1EncryptionAlgorithmsRequestListValue
  where
  toQuery
    Phase1EncryptionAlgorithmsRequestListValue' {..} =
      Prelude.mconcat ["Value" Prelude.=: value]
