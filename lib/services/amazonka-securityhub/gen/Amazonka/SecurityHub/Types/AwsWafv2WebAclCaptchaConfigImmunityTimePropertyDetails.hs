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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used for CAPTCHA and challenge token settings. Determines how long a
-- CAPTCHA or challenge timestamp remains valid after WAF updates it for a
-- successful CAPTCHA or challenge response.
--
-- /See:/ 'newAwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' smart constructor.
data AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails = AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails'
  { -- | The amount of time, in seconds, that a CAPTCHA or challenge timestamp is
    -- considered valid by WAF.
    immunityTime :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'immunityTime', 'awsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails_immunityTime' - The amount of time, in seconds, that a CAPTCHA or challenge timestamp is
-- considered valid by WAF.
newAwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails ::
  AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
newAwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails =
  AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails'
    { immunityTime =
        Prelude.Nothing
    }

-- | The amount of time, in seconds, that a CAPTCHA or challenge timestamp is
-- considered valid by WAF.
awsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails_immunityTime :: Lens.Lens' AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails (Prelude.Maybe Prelude.Integer)
awsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails_immunityTime = Lens.lens (\AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' {immunityTime} -> immunityTime) (\s@AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' {} a -> s {immunityTime = a} :: AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails)

instance
  Data.FromJSON
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails"
      ( \x ->
          AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails'
            Prelude.<$> (x Data..:? "ImmunityTime")
      )

instance
  Prelude.Hashable
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
  where
  hashWithSalt
    _salt
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' {..} =
      _salt `Prelude.hashWithSalt` immunityTime

instance
  Prelude.NFData
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
  where
  rnf
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' {..} =
      Prelude.rnf immunityTime

instance
  Data.ToJSON
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
  where
  toJSON
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("ImmunityTime" Data..=) Prelude.<$> immunityTime]
        )
