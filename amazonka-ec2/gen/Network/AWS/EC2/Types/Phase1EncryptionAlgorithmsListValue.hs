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
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'newPhase1EncryptionAlgorithmsListValue' smart constructor.
data Phase1EncryptionAlgorithmsListValue = Phase1EncryptionAlgorithmsListValue'
  { -- | The value for the encryption algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase1EncryptionAlgorithmsListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1EncryptionAlgorithmsListValue_value' - The value for the encryption algorithm.
newPhase1EncryptionAlgorithmsListValue ::
  Phase1EncryptionAlgorithmsListValue
newPhase1EncryptionAlgorithmsListValue =
  Phase1EncryptionAlgorithmsListValue'
    { value =
        Prelude.Nothing
    }

-- | The value for the encryption algorithm.
phase1EncryptionAlgorithmsListValue_value :: Lens.Lens' Phase1EncryptionAlgorithmsListValue (Prelude.Maybe Prelude.Text)
phase1EncryptionAlgorithmsListValue_value = Lens.lens (\Phase1EncryptionAlgorithmsListValue' {value} -> value) (\s@Phase1EncryptionAlgorithmsListValue' {} a -> s {value = a} :: Phase1EncryptionAlgorithmsListValue)

instance
  Prelude.FromXML
    Phase1EncryptionAlgorithmsListValue
  where
  parseXML x =
    Phase1EncryptionAlgorithmsListValue'
      Prelude.<$> (x Prelude..@? "value")

instance
  Prelude.Hashable
    Phase1EncryptionAlgorithmsListValue

instance
  Prelude.NFData
    Phase1EncryptionAlgorithmsListValue
