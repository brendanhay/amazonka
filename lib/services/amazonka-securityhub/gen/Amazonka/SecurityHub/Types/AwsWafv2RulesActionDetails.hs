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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2RulesActionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2RulesActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails
import Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCaptchaDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCountDetails

-- | The action that WAF should take on a web request when it matches a
-- rule\'s statement. Settings at the web ACL level can override the rule
-- action setting.
--
-- /See:/ 'newAwsWafv2RulesActionDetails' smart constructor.
data AwsWafv2RulesActionDetails = AwsWafv2RulesActionDetails'
  { -- | Instructs WAF to allow the web request.
    allow :: Prelude.Maybe AwsWafv2ActionAllowDetails,
    -- | Instructs WAF to block the web request.
    block :: Prelude.Maybe AwsWafv2ActionBlockDetails,
    -- | Instructs WAF to run a CAPTCHA check against the web request.
    captcha :: Prelude.Maybe AwsWafv2RulesActionCaptchaDetails,
    -- | Instructs WAF to count the web request and then continue evaluating the
    -- request using the remaining rules in the web ACL.
    count :: Prelude.Maybe AwsWafv2RulesActionCountDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2RulesActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allow', 'awsWafv2RulesActionDetails_allow' - Instructs WAF to allow the web request.
--
-- 'block', 'awsWafv2RulesActionDetails_block' - Instructs WAF to block the web request.
--
-- 'captcha', 'awsWafv2RulesActionDetails_captcha' - Instructs WAF to run a CAPTCHA check against the web request.
--
-- 'count', 'awsWafv2RulesActionDetails_count' - Instructs WAF to count the web request and then continue evaluating the
-- request using the remaining rules in the web ACL.
newAwsWafv2RulesActionDetails ::
  AwsWafv2RulesActionDetails
newAwsWafv2RulesActionDetails =
  AwsWafv2RulesActionDetails'
    { allow =
        Prelude.Nothing,
      block = Prelude.Nothing,
      captcha = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Instructs WAF to allow the web request.
awsWafv2RulesActionDetails_allow :: Lens.Lens' AwsWafv2RulesActionDetails (Prelude.Maybe AwsWafv2ActionAllowDetails)
awsWafv2RulesActionDetails_allow = Lens.lens (\AwsWafv2RulesActionDetails' {allow} -> allow) (\s@AwsWafv2RulesActionDetails' {} a -> s {allow = a} :: AwsWafv2RulesActionDetails)

-- | Instructs WAF to block the web request.
awsWafv2RulesActionDetails_block :: Lens.Lens' AwsWafv2RulesActionDetails (Prelude.Maybe AwsWafv2ActionBlockDetails)
awsWafv2RulesActionDetails_block = Lens.lens (\AwsWafv2RulesActionDetails' {block} -> block) (\s@AwsWafv2RulesActionDetails' {} a -> s {block = a} :: AwsWafv2RulesActionDetails)

-- | Instructs WAF to run a CAPTCHA check against the web request.
awsWafv2RulesActionDetails_captcha :: Lens.Lens' AwsWafv2RulesActionDetails (Prelude.Maybe AwsWafv2RulesActionCaptchaDetails)
awsWafv2RulesActionDetails_captcha = Lens.lens (\AwsWafv2RulesActionDetails' {captcha} -> captcha) (\s@AwsWafv2RulesActionDetails' {} a -> s {captcha = a} :: AwsWafv2RulesActionDetails)

-- | Instructs WAF to count the web request and then continue evaluating the
-- request using the remaining rules in the web ACL.
awsWafv2RulesActionDetails_count :: Lens.Lens' AwsWafv2RulesActionDetails (Prelude.Maybe AwsWafv2RulesActionCountDetails)
awsWafv2RulesActionDetails_count = Lens.lens (\AwsWafv2RulesActionDetails' {count} -> count) (\s@AwsWafv2RulesActionDetails' {} a -> s {count = a} :: AwsWafv2RulesActionDetails)

instance Data.FromJSON AwsWafv2RulesActionDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2RulesActionDetails"
      ( \x ->
          AwsWafv2RulesActionDetails'
            Prelude.<$> (x Data..:? "Allow")
            Prelude.<*> (x Data..:? "Block")
            Prelude.<*> (x Data..:? "Captcha")
            Prelude.<*> (x Data..:? "Count")
      )

instance Prelude.Hashable AwsWafv2RulesActionDetails where
  hashWithSalt _salt AwsWafv2RulesActionDetails' {..} =
    _salt `Prelude.hashWithSalt` allow
      `Prelude.hashWithSalt` block
      `Prelude.hashWithSalt` captcha
      `Prelude.hashWithSalt` count

instance Prelude.NFData AwsWafv2RulesActionDetails where
  rnf AwsWafv2RulesActionDetails' {..} =
    Prelude.rnf allow
      `Prelude.seq` Prelude.rnf block
      `Prelude.seq` Prelude.rnf captcha
      `Prelude.seq` Prelude.rnf count

instance Data.ToJSON AwsWafv2RulesActionDetails where
  toJSON AwsWafv2RulesActionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Allow" Data..=) Prelude.<$> allow,
            ("Block" Data..=) Prelude.<$> block,
            ("Captcha" Data..=) Prelude.<$> captcha,
            ("Count" Data..=) Prelude.<$> count
          ]
      )
