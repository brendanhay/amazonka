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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2WebAclActionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2WebAclActionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails
import Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails

-- | Specifies the action that Amazon CloudFront or WAF takes when a web
-- request matches the conditions in the rule.
--
-- /See:/ 'newAwsWafv2WebAclActionDetails' smart constructor.
data AwsWafv2WebAclActionDetails = AwsWafv2WebAclActionDetails'
  { -- | Specifies that WAF should allow requests by default.
    allow :: Prelude.Maybe AwsWafv2ActionAllowDetails,
    -- | Specifies that WAF should block requests by default.
    block :: Prelude.Maybe AwsWafv2ActionBlockDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2WebAclActionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allow', 'awsWafv2WebAclActionDetails_allow' - Specifies that WAF should allow requests by default.
--
-- 'block', 'awsWafv2WebAclActionDetails_block' - Specifies that WAF should block requests by default.
newAwsWafv2WebAclActionDetails ::
  AwsWafv2WebAclActionDetails
newAwsWafv2WebAclActionDetails =
  AwsWafv2WebAclActionDetails'
    { allow =
        Prelude.Nothing,
      block = Prelude.Nothing
    }

-- | Specifies that WAF should allow requests by default.
awsWafv2WebAclActionDetails_allow :: Lens.Lens' AwsWafv2WebAclActionDetails (Prelude.Maybe AwsWafv2ActionAllowDetails)
awsWafv2WebAclActionDetails_allow = Lens.lens (\AwsWafv2WebAclActionDetails' {allow} -> allow) (\s@AwsWafv2WebAclActionDetails' {} a -> s {allow = a} :: AwsWafv2WebAclActionDetails)

-- | Specifies that WAF should block requests by default.
awsWafv2WebAclActionDetails_block :: Lens.Lens' AwsWafv2WebAclActionDetails (Prelude.Maybe AwsWafv2ActionBlockDetails)
awsWafv2WebAclActionDetails_block = Lens.lens (\AwsWafv2WebAclActionDetails' {block} -> block) (\s@AwsWafv2WebAclActionDetails' {} a -> s {block = a} :: AwsWafv2WebAclActionDetails)

instance Data.FromJSON AwsWafv2WebAclActionDetails where
  parseJSON =
    Data.withObject
      "AwsWafv2WebAclActionDetails"
      ( \x ->
          AwsWafv2WebAclActionDetails'
            Prelude.<$> (x Data..:? "Allow")
            Prelude.<*> (x Data..:? "Block")
      )

instance Prelude.Hashable AwsWafv2WebAclActionDetails where
  hashWithSalt _salt AwsWafv2WebAclActionDetails' {..} =
    _salt `Prelude.hashWithSalt` allow
      `Prelude.hashWithSalt` block

instance Prelude.NFData AwsWafv2WebAclActionDetails where
  rnf AwsWafv2WebAclActionDetails' {..} =
    Prelude.rnf allow `Prelude.seq` Prelude.rnf block

instance Data.ToJSON AwsWafv2WebAclActionDetails where
  toJSON AwsWafv2WebAclActionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Allow" Data..=) Prelude.<$> allow,
            ("Block" Data..=) Prelude.<$> block
          ]
      )
