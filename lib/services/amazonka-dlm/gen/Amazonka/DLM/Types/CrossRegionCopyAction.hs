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
-- Module      : Amazonka.DLM.Types.CrossRegionCopyAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CrossRegionCopyAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.CrossRegionCopyRetainRule
import Amazonka.DLM.Types.EncryptionConfiguration
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Event-based policies only]__ Specifies a cross-Region copy action for
-- event-based policies.
--
-- To specify a cross-Region copy rule for snapshot and AMI policies, use
-- CrossRegionCopyRule.
--
-- /See:/ 'newCrossRegionCopyAction' smart constructor.
data CrossRegionCopyAction = CrossRegionCopyAction'
  { retainRule :: Prelude.Maybe CrossRegionCopyRetainRule,
    -- | The target Region.
    target :: Prelude.Text,
    -- | The encryption settings for the copied snapshot.
    encryptionConfiguration :: EncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossRegionCopyAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retainRule', 'crossRegionCopyAction_retainRule' - Undocumented member.
--
-- 'target', 'crossRegionCopyAction_target' - The target Region.
--
-- 'encryptionConfiguration', 'crossRegionCopyAction_encryptionConfiguration' - The encryption settings for the copied snapshot.
newCrossRegionCopyAction ::
  -- | 'target'
  Prelude.Text ->
  -- | 'encryptionConfiguration'
  EncryptionConfiguration ->
  CrossRegionCopyAction
newCrossRegionCopyAction
  pTarget_
  pEncryptionConfiguration_ =
    CrossRegionCopyAction'
      { retainRule =
          Prelude.Nothing,
        target = pTarget_,
        encryptionConfiguration = pEncryptionConfiguration_
      }

-- | Undocumented member.
crossRegionCopyAction_retainRule :: Lens.Lens' CrossRegionCopyAction (Prelude.Maybe CrossRegionCopyRetainRule)
crossRegionCopyAction_retainRule = Lens.lens (\CrossRegionCopyAction' {retainRule} -> retainRule) (\s@CrossRegionCopyAction' {} a -> s {retainRule = a} :: CrossRegionCopyAction)

-- | The target Region.
crossRegionCopyAction_target :: Lens.Lens' CrossRegionCopyAction Prelude.Text
crossRegionCopyAction_target = Lens.lens (\CrossRegionCopyAction' {target} -> target) (\s@CrossRegionCopyAction' {} a -> s {target = a} :: CrossRegionCopyAction)

-- | The encryption settings for the copied snapshot.
crossRegionCopyAction_encryptionConfiguration :: Lens.Lens' CrossRegionCopyAction EncryptionConfiguration
crossRegionCopyAction_encryptionConfiguration = Lens.lens (\CrossRegionCopyAction' {encryptionConfiguration} -> encryptionConfiguration) (\s@CrossRegionCopyAction' {} a -> s {encryptionConfiguration = a} :: CrossRegionCopyAction)

instance Data.FromJSON CrossRegionCopyAction where
  parseJSON =
    Data.withObject
      "CrossRegionCopyAction"
      ( \x ->
          CrossRegionCopyAction'
            Prelude.<$> (x Data..:? "RetainRule")
            Prelude.<*> (x Data..: "Target")
            Prelude.<*> (x Data..: "EncryptionConfiguration")
      )

instance Prelude.Hashable CrossRegionCopyAction where
  hashWithSalt _salt CrossRegionCopyAction' {..} =
    _salt `Prelude.hashWithSalt` retainRule
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` encryptionConfiguration

instance Prelude.NFData CrossRegionCopyAction where
  rnf CrossRegionCopyAction' {..} =
    Prelude.rnf retainRule
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf encryptionConfiguration

instance Data.ToJSON CrossRegionCopyAction where
  toJSON CrossRegionCopyAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RetainRule" Data..=) Prelude.<$> retainRule,
            Prelude.Just ("Target" Data..= target),
            Prelude.Just
              ( "EncryptionConfiguration"
                  Data..= encryptionConfiguration
              )
          ]
      )
