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
-- Module      : Amazonka.WorkMail.Types.MailDomainSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MailDomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data for a given domain.
--
-- /See:/ 'newMailDomainSummary' smart constructor.
data MailDomainSummary = MailDomainSummary'
  { -- | Whether the domain is default or not.
    defaultDomain :: Prelude.Maybe Prelude.Bool,
    -- | The domain name.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MailDomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultDomain', 'mailDomainSummary_defaultDomain' - Whether the domain is default or not.
--
-- 'domainName', 'mailDomainSummary_domainName' - The domain name.
newMailDomainSummary ::
  MailDomainSummary
newMailDomainSummary =
  MailDomainSummary'
    { defaultDomain = Prelude.Nothing,
      domainName = Prelude.Nothing
    }

-- | Whether the domain is default or not.
mailDomainSummary_defaultDomain :: Lens.Lens' MailDomainSummary (Prelude.Maybe Prelude.Bool)
mailDomainSummary_defaultDomain = Lens.lens (\MailDomainSummary' {defaultDomain} -> defaultDomain) (\s@MailDomainSummary' {} a -> s {defaultDomain = a} :: MailDomainSummary)

-- | The domain name.
mailDomainSummary_domainName :: Lens.Lens' MailDomainSummary (Prelude.Maybe Prelude.Text)
mailDomainSummary_domainName = Lens.lens (\MailDomainSummary' {domainName} -> domainName) (\s@MailDomainSummary' {} a -> s {domainName = a} :: MailDomainSummary)

instance Data.FromJSON MailDomainSummary where
  parseJSON =
    Data.withObject
      "MailDomainSummary"
      ( \x ->
          MailDomainSummary'
            Prelude.<$> (x Data..:? "DefaultDomain")
            Prelude.<*> (x Data..:? "DomainName")
      )

instance Prelude.Hashable MailDomainSummary where
  hashWithSalt _salt MailDomainSummary' {..} =
    _salt `Prelude.hashWithSalt` defaultDomain
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData MailDomainSummary where
  rnf MailDomainSummary' {..} =
    Prelude.rnf defaultDomain
      `Prelude.seq` Prelude.rnf domainName
