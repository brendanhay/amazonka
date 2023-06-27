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
-- Module      : Amazonka.WAFRegional.Types.IPSetDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFRegional.Types.IPSetDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFRegional.Types.IPSetDescriptorType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the IP address type (@IPV4@ or @IPV6@) and the IP address
-- range (in CIDR format) that web requests originate from.
--
-- /See:/ 'newIPSetDescriptor' smart constructor.
data IPSetDescriptor = IPSetDescriptor'
  { -- | Specify @IPV4@ or @IPV6@.
    type' :: IPSetDescriptorType,
    -- | Specify an IPv4 address by using CIDR notation. For example:
    --
    -- -   To configure AWS WAF to allow, block, or count requests that
    --     originated from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure AWS WAF to allow, block, or count requests that
    --     originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify
    --     @192.0.2.0\/24@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    --
    -- Specify an IPv6 address by using CIDR notation. For example:
    --
    -- -   To configure AWS WAF to allow, block, or count requests that
    --     originated from the IP address
    --     1111:0000:0000:0000:0000:0000:0000:0111, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
    --
    -- -   To configure AWS WAF to allow, block, or count requests that
    --     originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000
    --     to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'iPSetDescriptor_type' - Specify @IPV4@ or @IPV6@.
--
-- 'value', 'iPSetDescriptor_value' - Specify an IPv4 address by using CIDR notation. For example:
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Specify an IPv6 address by using CIDR notation. For example:
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from the IP address
--     1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000
--     to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
newIPSetDescriptor ::
  -- | 'type''
  IPSetDescriptorType ->
  -- | 'value'
  Prelude.Text ->
  IPSetDescriptor
newIPSetDescriptor pType_ pValue_ =
  IPSetDescriptor' {type' = pType_, value = pValue_}

-- | Specify @IPV4@ or @IPV6@.
iPSetDescriptor_type :: Lens.Lens' IPSetDescriptor IPSetDescriptorType
iPSetDescriptor_type = Lens.lens (\IPSetDescriptor' {type'} -> type') (\s@IPSetDescriptor' {} a -> s {type' = a} :: IPSetDescriptor)

-- | Specify an IPv4 address by using CIDR notation. For example:
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Specify an IPv6 address by using CIDR notation. For example:
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from the IP address
--     1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure AWS WAF to allow, block, or count requests that
--     originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000
--     to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
iPSetDescriptor_value :: Lens.Lens' IPSetDescriptor Prelude.Text
iPSetDescriptor_value = Lens.lens (\IPSetDescriptor' {value} -> value) (\s@IPSetDescriptor' {} a -> s {value = a} :: IPSetDescriptor)

instance Data.FromJSON IPSetDescriptor where
  parseJSON =
    Data.withObject
      "IPSetDescriptor"
      ( \x ->
          IPSetDescriptor'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable IPSetDescriptor where
  hashWithSalt _salt IPSetDescriptor' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData IPSetDescriptor where
  rnf IPSetDescriptor' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON IPSetDescriptor where
  toJSON IPSetDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Value" Data..= value)
          ]
      )
