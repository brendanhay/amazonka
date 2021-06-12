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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The encryption algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'newPhase2EncryptionAlgorithmsListValue' smart constructor.
data Phase2EncryptionAlgorithmsListValue = Phase2EncryptionAlgorithmsListValue'
  { -- | The encryption algorithm.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The encryption algorithm.
phase2EncryptionAlgorithmsListValue_value :: Lens.Lens' Phase2EncryptionAlgorithmsListValue (Core.Maybe Core.Text)
phase2EncryptionAlgorithmsListValue_value = Lens.lens (\Phase2EncryptionAlgorithmsListValue' {value} -> value) (\s@Phase2EncryptionAlgorithmsListValue' {} a -> s {value = a} :: Phase2EncryptionAlgorithmsListValue)

instance
  Core.FromXML
    Phase2EncryptionAlgorithmsListValue
  where
  parseXML x =
    Phase2EncryptionAlgorithmsListValue'
      Core.<$> (x Core..@? "value")

instance
  Core.Hashable
    Phase2EncryptionAlgorithmsListValue

instance
  Core.NFData
    Phase2EncryptionAlgorithmsListValue
