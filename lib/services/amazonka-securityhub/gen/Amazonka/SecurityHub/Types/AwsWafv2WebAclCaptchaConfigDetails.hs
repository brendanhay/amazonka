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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails

-- | Specifies how WAF should handle CAPTCHA evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings.
--
-- /See:/ 'newAwsWafv2WebAclCaptchaConfigDetails' smart constructor.
data AwsWafv2WebAclCaptchaConfigDetails = AwsWafv2WebAclCaptchaConfigDetails'
  { -- | Determines how long a CAPTCHA timestamp in the token remains valid after
    -- the client successfully solves a CAPTCHA puzzle.
    immunityTimeProperty :: Prelude.Maybe AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2WebAclCaptchaConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'immunityTimeProperty', 'awsWafv2WebAclCaptchaConfigDetails_immunityTimeProperty' - Determines how long a CAPTCHA timestamp in the token remains valid after
-- the client successfully solves a CAPTCHA puzzle.
newAwsWafv2WebAclCaptchaConfigDetails ::
  AwsWafv2WebAclCaptchaConfigDetails
newAwsWafv2WebAclCaptchaConfigDetails =
  AwsWafv2WebAclCaptchaConfigDetails'
    { immunityTimeProperty =
        Prelude.Nothing
    }

-- | Determines how long a CAPTCHA timestamp in the token remains valid after
-- the client successfully solves a CAPTCHA puzzle.
awsWafv2WebAclCaptchaConfigDetails_immunityTimeProperty :: Lens.Lens' AwsWafv2WebAclCaptchaConfigDetails (Prelude.Maybe AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails)
awsWafv2WebAclCaptchaConfigDetails_immunityTimeProperty = Lens.lens (\AwsWafv2WebAclCaptchaConfigDetails' {immunityTimeProperty} -> immunityTimeProperty) (\s@AwsWafv2WebAclCaptchaConfigDetails' {} a -> s {immunityTimeProperty = a} :: AwsWafv2WebAclCaptchaConfigDetails)

instance
  Data.FromJSON
    AwsWafv2WebAclCaptchaConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2WebAclCaptchaConfigDetails"
      ( \x ->
          AwsWafv2WebAclCaptchaConfigDetails'
            Prelude.<$> (x Data..:? "ImmunityTimeProperty")
      )

instance
  Prelude.Hashable
    AwsWafv2WebAclCaptchaConfigDetails
  where
  hashWithSalt
    _salt
    AwsWafv2WebAclCaptchaConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` immunityTimeProperty

instance
  Prelude.NFData
    AwsWafv2WebAclCaptchaConfigDetails
  where
  rnf AwsWafv2WebAclCaptchaConfigDetails' {..} =
    Prelude.rnf immunityTimeProperty

instance
  Data.ToJSON
    AwsWafv2WebAclCaptchaConfigDetails
  where
  toJSON AwsWafv2WebAclCaptchaConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImmunityTimeProperty" Data..=)
              Prelude.<$> immunityTimeProperty
          ]
      )
