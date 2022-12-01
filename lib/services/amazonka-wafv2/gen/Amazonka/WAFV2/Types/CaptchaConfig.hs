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
-- Module      : Amazonka.WAFV2.Types.CaptchaConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CaptchaConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ImmunityTimeProperty

-- | Specifies how WAF should handle @CAPTCHA@ evaluations. This is available
-- at the web ACL level and in each rule.
--
-- /See:/ 'newCaptchaConfig' smart constructor.
data CaptchaConfig = CaptchaConfig'
  { -- | Determines how long a @CAPTCHA@ timestamp in the token remains valid
    -- after the client successfully solves a @CAPTCHA@ puzzle.
    immunityTimeProperty :: Prelude.Maybe ImmunityTimeProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptchaConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'immunityTimeProperty', 'captchaConfig_immunityTimeProperty' - Determines how long a @CAPTCHA@ timestamp in the token remains valid
-- after the client successfully solves a @CAPTCHA@ puzzle.
newCaptchaConfig ::
  CaptchaConfig
newCaptchaConfig =
  CaptchaConfig'
    { immunityTimeProperty =
        Prelude.Nothing
    }

-- | Determines how long a @CAPTCHA@ timestamp in the token remains valid
-- after the client successfully solves a @CAPTCHA@ puzzle.
captchaConfig_immunityTimeProperty :: Lens.Lens' CaptchaConfig (Prelude.Maybe ImmunityTimeProperty)
captchaConfig_immunityTimeProperty = Lens.lens (\CaptchaConfig' {immunityTimeProperty} -> immunityTimeProperty) (\s@CaptchaConfig' {} a -> s {immunityTimeProperty = a} :: CaptchaConfig)

instance Core.FromJSON CaptchaConfig where
  parseJSON =
    Core.withObject
      "CaptchaConfig"
      ( \x ->
          CaptchaConfig'
            Prelude.<$> (x Core..:? "ImmunityTimeProperty")
      )

instance Prelude.Hashable CaptchaConfig where
  hashWithSalt _salt CaptchaConfig' {..} =
    _salt `Prelude.hashWithSalt` immunityTimeProperty

instance Prelude.NFData CaptchaConfig where
  rnf CaptchaConfig' {..} =
    Prelude.rnf immunityTimeProperty

instance Core.ToJSON CaptchaConfig where
  toJSON CaptchaConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ImmunityTimeProperty" Core..=)
              Prelude.<$> immunityTimeProperty
          ]
      )
