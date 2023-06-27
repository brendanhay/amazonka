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
-- Module      : Amazonka.WAFV2.Types.IPSetReferenceStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.IPSetReferenceStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.IPSetForwardedIPConfig

-- | A rule statement used to detect web requests coming from particular IP
-- addresses or address ranges. To use this, create an IPSet that specifies
-- the addresses you want to detect, then use the ARN of that set in this
-- statement. To create an IP set, see CreateIPSet.
--
-- Each IP set rule statement references an IP set. You create and maintain
-- the set independent of your rules. This allows you to use the single set
-- in multiple rules. When you update the referenced set, WAF automatically
-- updates all rules that reference it.
--
-- /See:/ 'newIPSetReferenceStatement' smart constructor.
data IPSetReferenceStatement = IPSetReferenceStatement'
  { -- | The configuration for inspecting IP addresses in an HTTP header that you
    -- specify, instead of using the IP address that\'s reported by the web
    -- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
    -- you can specify any header name.
    --
    -- If the specified header isn\'t present in the request, WAF doesn\'t
    -- apply the rule to the web request at all.
    iPSetForwardedIPConfig :: Prelude.Maybe IPSetForwardedIPConfig,
    -- | The Amazon Resource Name (ARN) of the IPSet that this statement
    -- references.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetReferenceStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSetForwardedIPConfig', 'iPSetReferenceStatement_iPSetForwardedIPConfig' - The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
--
-- 'arn', 'iPSetReferenceStatement_arn' - The Amazon Resource Name (ARN) of the IPSet that this statement
-- references.
newIPSetReferenceStatement ::
  -- | 'arn'
  Prelude.Text ->
  IPSetReferenceStatement
newIPSetReferenceStatement pARN_ =
  IPSetReferenceStatement'
    { iPSetForwardedIPConfig =
        Prelude.Nothing,
      arn = pARN_
    }

-- | The configuration for inspecting IP addresses in an HTTP header that you
-- specify, instead of using the IP address that\'s reported by the web
-- request origin. Commonly, this is the X-Forwarded-For (XFF) header, but
-- you can specify any header name.
--
-- If the specified header isn\'t present in the request, WAF doesn\'t
-- apply the rule to the web request at all.
iPSetReferenceStatement_iPSetForwardedIPConfig :: Lens.Lens' IPSetReferenceStatement (Prelude.Maybe IPSetForwardedIPConfig)
iPSetReferenceStatement_iPSetForwardedIPConfig = Lens.lens (\IPSetReferenceStatement' {iPSetForwardedIPConfig} -> iPSetForwardedIPConfig) (\s@IPSetReferenceStatement' {} a -> s {iPSetForwardedIPConfig = a} :: IPSetReferenceStatement)

-- | The Amazon Resource Name (ARN) of the IPSet that this statement
-- references.
iPSetReferenceStatement_arn :: Lens.Lens' IPSetReferenceStatement Prelude.Text
iPSetReferenceStatement_arn = Lens.lens (\IPSetReferenceStatement' {arn} -> arn) (\s@IPSetReferenceStatement' {} a -> s {arn = a} :: IPSetReferenceStatement)

instance Data.FromJSON IPSetReferenceStatement where
  parseJSON =
    Data.withObject
      "IPSetReferenceStatement"
      ( \x ->
          IPSetReferenceStatement'
            Prelude.<$> (x Data..:? "IPSetForwardedIPConfig")
            Prelude.<*> (x Data..: "ARN")
      )

instance Prelude.Hashable IPSetReferenceStatement where
  hashWithSalt _salt IPSetReferenceStatement' {..} =
    _salt
      `Prelude.hashWithSalt` iPSetForwardedIPConfig
      `Prelude.hashWithSalt` arn

instance Prelude.NFData IPSetReferenceStatement where
  rnf IPSetReferenceStatement' {..} =
    Prelude.rnf iPSetForwardedIPConfig
      `Prelude.seq` Prelude.rnf arn

instance Data.ToJSON IPSetReferenceStatement where
  toJSON IPSetReferenceStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IPSetForwardedIPConfig" Data..=)
              Prelude.<$> iPSetForwardedIPConfig,
            Prelude.Just ("ARN" Data..= arn)
          ]
      )
