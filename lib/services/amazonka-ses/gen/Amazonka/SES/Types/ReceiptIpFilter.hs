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
-- Module      : Amazonka.SES.Types.ReceiptIpFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptIpFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.ReceiptFilterPolicy

-- | A receipt IP address filter enables you to specify whether to accept or
-- reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptIpFilter' smart constructor.
data ReceiptIpFilter = ReceiptIpFilter'
  { -- | Indicates whether to block or allow incoming mail from the specified IP
    -- addresses.
    policy :: ReceiptFilterPolicy,
    -- | A single IP address or a range of IP addresses that you want to block or
    -- allow, specified in Classless Inter-Domain Routing (CIDR) notation. An
    -- example of a single email address is 10.0.0.1. An example of a range of
    -- IP addresses is 10.0.0.1\/24. For more information about CIDR notation,
    -- see <https://tools.ietf.org/html/rfc2317 RFC 2317>.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiptIpFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'receiptIpFilter_policy' - Indicates whether to block or allow incoming mail from the specified IP
-- addresses.
--
-- 'cidr', 'receiptIpFilter_cidr' - A single IP address or a range of IP addresses that you want to block or
-- allow, specified in Classless Inter-Domain Routing (CIDR) notation. An
-- example of a single email address is 10.0.0.1. An example of a range of
-- IP addresses is 10.0.0.1\/24. For more information about CIDR notation,
-- see <https://tools.ietf.org/html/rfc2317 RFC 2317>.
newReceiptIpFilter ::
  -- | 'policy'
  ReceiptFilterPolicy ->
  -- | 'cidr'
  Prelude.Text ->
  ReceiptIpFilter
newReceiptIpFilter pPolicy_ pCidr_ =
  ReceiptIpFilter' {policy = pPolicy_, cidr = pCidr_}

-- | Indicates whether to block or allow incoming mail from the specified IP
-- addresses.
receiptIpFilter_policy :: Lens.Lens' ReceiptIpFilter ReceiptFilterPolicy
receiptIpFilter_policy = Lens.lens (\ReceiptIpFilter' {policy} -> policy) (\s@ReceiptIpFilter' {} a -> s {policy = a} :: ReceiptIpFilter)

-- | A single IP address or a range of IP addresses that you want to block or
-- allow, specified in Classless Inter-Domain Routing (CIDR) notation. An
-- example of a single email address is 10.0.0.1. An example of a range of
-- IP addresses is 10.0.0.1\/24. For more information about CIDR notation,
-- see <https://tools.ietf.org/html/rfc2317 RFC 2317>.
receiptIpFilter_cidr :: Lens.Lens' ReceiptIpFilter Prelude.Text
receiptIpFilter_cidr = Lens.lens (\ReceiptIpFilter' {cidr} -> cidr) (\s@ReceiptIpFilter' {} a -> s {cidr = a} :: ReceiptIpFilter)

instance Data.FromXML ReceiptIpFilter where
  parseXML x =
    ReceiptIpFilter'
      Prelude.<$> (x Data..@ "Policy")
      Prelude.<*> (x Data..@ "Cidr")

instance Prelude.Hashable ReceiptIpFilter where
  hashWithSalt _salt ReceiptIpFilter' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` cidr

instance Prelude.NFData ReceiptIpFilter where
  rnf ReceiptIpFilter' {..} =
    Prelude.rnf policy `Prelude.seq` Prelude.rnf cidr

instance Data.ToQuery ReceiptIpFilter where
  toQuery ReceiptIpFilter' {..} =
    Prelude.mconcat
      ["Policy" Data.=: policy, "Cidr" Data.=: cidr]
