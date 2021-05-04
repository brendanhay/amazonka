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
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'newPhase2EncryptionAlgorithmsListValue' smart constructor.
data Phase2EncryptionAlgorithmsListValue = Phase2EncryptionAlgorithmsListValue'
  { -- | The encryption algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase2EncryptionAlgorithmsListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase2EncryptionAlgorithmsListValue_value' - The encryption algorithm.
newPhase2EncryptionAlgorithmsListValue ::
  Phase2EncryptionAlgorithmsListValue
newPhase2EncryptionAlgorithmsListValue =
  Phase2EncryptionAlgorithmsListValue'
    { value =
        Prelude.Nothing
    }

-- | The encryption algorithm.
phase2EncryptionAlgorithmsListValue_value :: Lens.Lens' Phase2EncryptionAlgorithmsListValue (Prelude.Maybe Prelude.Text)
phase2EncryptionAlgorithmsListValue_value = Lens.lens (\Phase2EncryptionAlgorithmsListValue' {value} -> value) (\s@Phase2EncryptionAlgorithmsListValue' {} a -> s {value = a} :: Phase2EncryptionAlgorithmsListValue)

instance
  Prelude.FromXML
    Phase2EncryptionAlgorithmsListValue
  where
  parseXML x =
    Phase2EncryptionAlgorithmsListValue'
      Prelude.<$> (x Prelude..@? "value")

instance
  Prelude.Hashable
    Phase2EncryptionAlgorithmsListValue

instance
  Prelude.NFData
    Phase2EncryptionAlgorithmsListValue
