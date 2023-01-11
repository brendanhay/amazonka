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
-- Module      : Amazonka.WAFV2.Types.ChallengeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ChallengeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ImmunityTimeProperty

-- | Specifies how WAF should handle @Challenge@ evaluations. This is
-- available at the web ACL level and in each rule.
--
-- /See:/ 'newChallengeConfig' smart constructor.
data ChallengeConfig = ChallengeConfig'
  { -- | Determines how long a challenge timestamp in the token remains valid
    -- after the client successfully responds to a challenge.
    immunityTimeProperty :: Prelude.Maybe ImmunityTimeProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChallengeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'immunityTimeProperty', 'challengeConfig_immunityTimeProperty' - Determines how long a challenge timestamp in the token remains valid
-- after the client successfully responds to a challenge.
newChallengeConfig ::
  ChallengeConfig
newChallengeConfig =
  ChallengeConfig'
    { immunityTimeProperty =
        Prelude.Nothing
    }

-- | Determines how long a challenge timestamp in the token remains valid
-- after the client successfully responds to a challenge.
challengeConfig_immunityTimeProperty :: Lens.Lens' ChallengeConfig (Prelude.Maybe ImmunityTimeProperty)
challengeConfig_immunityTimeProperty = Lens.lens (\ChallengeConfig' {immunityTimeProperty} -> immunityTimeProperty) (\s@ChallengeConfig' {} a -> s {immunityTimeProperty = a} :: ChallengeConfig)

instance Data.FromJSON ChallengeConfig where
  parseJSON =
    Data.withObject
      "ChallengeConfig"
      ( \x ->
          ChallengeConfig'
            Prelude.<$> (x Data..:? "ImmunityTimeProperty")
      )

instance Prelude.Hashable ChallengeConfig where
  hashWithSalt _salt ChallengeConfig' {..} =
    _salt `Prelude.hashWithSalt` immunityTimeProperty

instance Prelude.NFData ChallengeConfig where
  rnf ChallengeConfig' {..} =
    Prelude.rnf immunityTimeProperty

instance Data.ToJSON ChallengeConfig where
  toJSON ChallengeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImmunityTimeProperty" Data..=)
              Prelude.<$> immunityTimeProperty
          ]
      )
