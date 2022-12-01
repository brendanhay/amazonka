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
-- Module      : Amazonka.VoiceId.Types.RegistrationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.RegistrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DuplicateRegistrationAction

-- | The configuration defining the action to take when a duplicate fraudster
-- is detected, and the similarity threshold to use for detecting a
-- duplicate fraudster during a batch fraudster registration job.
--
-- /See:/ 'newRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { -- | The action to take when a fraudster is identified as a duplicate. The
    -- default action is @SKIP@, which skips registering the duplicate
    -- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
    -- fraudster into the specified domain.
    duplicateRegistrationAction :: Prelude.Maybe DuplicateRegistrationAction,
    -- | The minimum similarity score between the new and old fraudsters in order
    -- to consider the new fraudster a duplicate.
    fraudsterSimilarityThreshold :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duplicateRegistrationAction', 'registrationConfig_duplicateRegistrationAction' - The action to take when a fraudster is identified as a duplicate. The
-- default action is @SKIP@, which skips registering the duplicate
-- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
-- fraudster into the specified domain.
--
-- 'fraudsterSimilarityThreshold', 'registrationConfig_fraudsterSimilarityThreshold' - The minimum similarity score between the new and old fraudsters in order
-- to consider the new fraudster a duplicate.
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { duplicateRegistrationAction =
        Prelude.Nothing,
      fraudsterSimilarityThreshold = Prelude.Nothing
    }

-- | The action to take when a fraudster is identified as a duplicate. The
-- default action is @SKIP@, which skips registering the duplicate
-- fraudster. Setting the value to @REGISTER_AS_NEW@ always registers a new
-- fraudster into the specified domain.
registrationConfig_duplicateRegistrationAction :: Lens.Lens' RegistrationConfig (Prelude.Maybe DuplicateRegistrationAction)
registrationConfig_duplicateRegistrationAction = Lens.lens (\RegistrationConfig' {duplicateRegistrationAction} -> duplicateRegistrationAction) (\s@RegistrationConfig' {} a -> s {duplicateRegistrationAction = a} :: RegistrationConfig)

-- | The minimum similarity score between the new and old fraudsters in order
-- to consider the new fraudster a duplicate.
registrationConfig_fraudsterSimilarityThreshold :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Natural)
registrationConfig_fraudsterSimilarityThreshold = Lens.lens (\RegistrationConfig' {fraudsterSimilarityThreshold} -> fraudsterSimilarityThreshold) (\s@RegistrationConfig' {} a -> s {fraudsterSimilarityThreshold = a} :: RegistrationConfig)

instance Core.FromJSON RegistrationConfig where
  parseJSON =
    Core.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Core..:? "DuplicateRegistrationAction")
            Prelude.<*> (x Core..:? "FraudsterSimilarityThreshold")
      )

instance Prelude.Hashable RegistrationConfig where
  hashWithSalt _salt RegistrationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` duplicateRegistrationAction
      `Prelude.hashWithSalt` fraudsterSimilarityThreshold

instance Prelude.NFData RegistrationConfig where
  rnf RegistrationConfig' {..} =
    Prelude.rnf duplicateRegistrationAction
      `Prelude.seq` Prelude.rnf fraudsterSimilarityThreshold

instance Core.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DuplicateRegistrationAction" Core..=)
              Prelude.<$> duplicateRegistrationAction,
            ("FraudsterSimilarityThreshold" Core..=)
              Prelude.<$> fraudsterSimilarityThreshold
          ]
      )
