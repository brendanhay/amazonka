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
-- Module      : Amazonka.WorkMail.Types.Domain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The domain to associate with an WorkMail organization.
--
-- When you configure a domain hosted in Amazon Route 53 (Route 53), all
-- recommended DNS records are added to the organization when you create
-- it. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain>
-- in the /WorkMail Administrator Guide/.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The fully qualified domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The hosted zone ID for a domain hosted in Route 53. Required when
    -- configuring a domain hosted in Route 53.
    hostedZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Domain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'domain_domainName' - The fully qualified domain name.
--
-- 'hostedZoneId', 'domain_hostedZoneId' - The hosted zone ID for a domain hosted in Route 53. Required when
-- configuring a domain hosted in Route 53.
newDomain ::
  Domain
newDomain =
  Domain'
    { domainName = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing
    }

-- | The fully qualified domain name.
domain_domainName :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_domainName = Lens.lens (\Domain' {domainName} -> domainName) (\s@Domain' {} a -> s {domainName = a} :: Domain)

-- | The hosted zone ID for a domain hosted in Route 53. Required when
-- configuring a domain hosted in Route 53.
domain_hostedZoneId :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_hostedZoneId = Lens.lens (\Domain' {hostedZoneId} -> hostedZoneId) (\s@Domain' {} a -> s {hostedZoneId = a} :: Domain)

instance Prelude.Hashable Domain where
  hashWithSalt _salt Domain' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData Domain where
  rnf Domain' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf hostedZoneId

instance Data.ToJSON Domain where
  toJSON Domain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DomainName" Data..=) Prelude.<$> domainName,
            ("HostedZoneId" Data..=) Prelude.<$> hostedZoneId
          ]
      )
