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
-- Module      : Amazonka.SES.Types.ReceiptFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.ReceiptIpFilter

-- | A receipt IP address filter enables you to specify whether to accept or
-- reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptFilter' smart constructor.
data ReceiptFilter = ReceiptFilter'
  { -- | The name of the IP address filter. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Start and end with a letter or number.
    --
    -- -   Contain less than 64 characters.
    name :: Prelude.Text,
    -- | A structure that provides the IP addresses to block or allow, and
    -- whether to block or allow incoming mail from them.
    ipFilter :: ReceiptIpFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'receiptFilter_name' - The name of the IP address filter. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
--
-- 'ipFilter', 'receiptFilter_ipFilter' - A structure that provides the IP addresses to block or allow, and
-- whether to block or allow incoming mail from them.
newReceiptFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ipFilter'
  ReceiptIpFilter ->
  ReceiptFilter
newReceiptFilter pName_ pIpFilter_ =
  ReceiptFilter'
    { name = pName_,
      ipFilter = pIpFilter_
    }

-- | The name of the IP address filter. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
receiptFilter_name :: Lens.Lens' ReceiptFilter Prelude.Text
receiptFilter_name = Lens.lens (\ReceiptFilter' {name} -> name) (\s@ReceiptFilter' {} a -> s {name = a} :: ReceiptFilter)

-- | A structure that provides the IP addresses to block or allow, and
-- whether to block or allow incoming mail from them.
receiptFilter_ipFilter :: Lens.Lens' ReceiptFilter ReceiptIpFilter
receiptFilter_ipFilter = Lens.lens (\ReceiptFilter' {ipFilter} -> ipFilter) (\s@ReceiptFilter' {} a -> s {ipFilter = a} :: ReceiptFilter)

instance Data.FromXML ReceiptFilter where
  parseXML x =
    ReceiptFilter'
      Prelude.<$> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "IpFilter")

instance Prelude.Hashable ReceiptFilter where
  hashWithSalt _salt ReceiptFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ipFilter

instance Prelude.NFData ReceiptFilter where
  rnf ReceiptFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf ipFilter

instance Data.ToQuery ReceiptFilter where
  toQuery ReceiptFilter' {..} =
    Prelude.mconcat
      ["Name" Data.=: name, "IpFilter" Data.=: ipFilter]
