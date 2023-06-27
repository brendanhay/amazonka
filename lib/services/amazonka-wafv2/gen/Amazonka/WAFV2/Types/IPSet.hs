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
-- Module      : Amazonka.WAFV2.Types.IPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.IPSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.IPAddressVersion

-- | Contains zero or more IP addresses or blocks of IP addresses specified
-- in Classless Inter-Domain Routing (CIDR) notation. WAF supports all IPv4
-- and IPv6 CIDR ranges except for \/0. For information about CIDR
-- notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- WAF assigns an ARN to each @IPSet@ that you create. To use an IP set in
-- a rule, you provide the ARN to the Rule statement
-- IPSetReferenceStatement.
--
-- /See:/ 'newIPSet' smart constructor.
data IPSet = IPSet'
  { -- | A description of the IP set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the IP set. You cannot change the name of an @IPSet@ after
    -- you create it.
    name :: Prelude.Text,
    -- | A unique identifier for the set. This ID is returned in the responses to
    -- create and list commands. You provide it to operations like update and
    -- delete.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Text,
    -- | The version of the IP addresses, either @IPV4@ or @IPV6@.
    iPAddressVersion :: IPAddressVersion,
    -- | Contains an array of strings that specifies zero or more IP addresses or
    -- blocks of IP addresses. All addresses must be specified using Classless
    -- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
    -- CIDR ranges except for @\/0@.
    --
    -- Example address strings:
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
    --     @192.0.2.0\/24@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
    --
    -- -   To configure WAF to allow, block, or count requests that originated
    --     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
    --     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
    --     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    --
    -- Example JSON @Addresses@ specifications:
    --
    -- -   Empty array: @\"Addresses\": []@
    --
    -- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
    --
    -- -   Array with three addresses:
    --     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
    --
    -- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
    addresses :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'iPSet_description' - A description of the IP set that helps with identification.
--
-- 'name', 'iPSet_name' - The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
--
-- 'id', 'iPSet_id' - A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
--
-- 'arn', 'iPSet_arn' - The Amazon Resource Name (ARN) of the entity.
--
-- 'iPAddressVersion', 'iPSet_iPAddressVersion' - The version of the IP addresses, either @IPV4@ or @IPV6@.
--
-- 'addresses', 'iPSet_addresses' - Contains an array of strings that specifies zero or more IP addresses or
-- blocks of IP addresses. All addresses must be specified using Classless
-- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
-- CIDR ranges except for @\/0@.
--
-- Example address strings:
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
--     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Example JSON @Addresses@ specifications:
--
-- -   Empty array: @\"Addresses\": []@
--
-- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
--
-- -   Array with three addresses:
--     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
--
-- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
newIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'iPAddressVersion'
  IPAddressVersion ->
  IPSet
newIPSet pName_ pId_ pARN_ pIPAddressVersion_ =
  IPSet'
    { description = Prelude.Nothing,
      name = pName_,
      id = pId_,
      arn = pARN_,
      iPAddressVersion = pIPAddressVersion_,
      addresses = Prelude.mempty
    }

-- | A description of the IP set that helps with identification.
iPSet_description :: Lens.Lens' IPSet (Prelude.Maybe Prelude.Text)
iPSet_description = Lens.lens (\IPSet' {description} -> description) (\s@IPSet' {} a -> s {description = a} :: IPSet)

-- | The name of the IP set. You cannot change the name of an @IPSet@ after
-- you create it.
iPSet_name :: Lens.Lens' IPSet Prelude.Text
iPSet_name = Lens.lens (\IPSet' {name} -> name) (\s@IPSet' {} a -> s {name = a} :: IPSet)

-- | A unique identifier for the set. This ID is returned in the responses to
-- create and list commands. You provide it to operations like update and
-- delete.
iPSet_id :: Lens.Lens' IPSet Prelude.Text
iPSet_id = Lens.lens (\IPSet' {id} -> id) (\s@IPSet' {} a -> s {id = a} :: IPSet)

-- | The Amazon Resource Name (ARN) of the entity.
iPSet_arn :: Lens.Lens' IPSet Prelude.Text
iPSet_arn = Lens.lens (\IPSet' {arn} -> arn) (\s@IPSet' {} a -> s {arn = a} :: IPSet)

-- | The version of the IP addresses, either @IPV4@ or @IPV6@.
iPSet_iPAddressVersion :: Lens.Lens' IPSet IPAddressVersion
iPSet_iPAddressVersion = Lens.lens (\IPSet' {iPAddressVersion} -> iPAddressVersion) (\s@IPSet' {} a -> s {iPAddressVersion = a} :: IPSet)

-- | Contains an array of strings that specifies zero or more IP addresses or
-- blocks of IP addresses. All addresses must be specified using Classless
-- Inter-Domain Routing (CIDR) notation. WAF supports all IPv4 and IPv6
-- CIDR ranges except for @\/0@.
--
-- Example address strings:
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses from 192.0.2.0 to 192.0.2.255, specify
--     @192.0.2.0\/24@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify
--     @1111:0000:0000:0000:0000:0000:0000:0111\/128@.
--
-- -   To configure WAF to allow, block, or count requests that originated
--     from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to
--     1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify
--     @1111:0000:0000:0000:0000:0000:0000:0000\/64@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- Example JSON @Addresses@ specifications:
--
-- -   Empty array: @\"Addresses\": []@
--
-- -   Array with one address: @\"Addresses\": [\"192.0.2.44\/32\"]@
--
-- -   Array with three addresses:
--     @\"Addresses\": [\"192.0.2.44\/32\", \"192.0.2.0\/24\", \"192.0.0.0\/16\"]@
--
-- -   INVALID specification: @\"Addresses\": [\"\"]@ INVALID
iPSet_addresses :: Lens.Lens' IPSet [Prelude.Text]
iPSet_addresses = Lens.lens (\IPSet' {addresses} -> addresses) (\s@IPSet' {} a -> s {addresses = a} :: IPSet) Prelude.. Lens.coerced

instance Data.FromJSON IPSet where
  parseJSON =
    Data.withObject
      "IPSet"
      ( \x ->
          IPSet'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ARN")
            Prelude.<*> (x Data..: "IPAddressVersion")
            Prelude.<*> (x Data..:? "Addresses" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable IPSet where
  hashWithSalt _salt IPSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` iPAddressVersion
      `Prelude.hashWithSalt` addresses

instance Prelude.NFData IPSet where
  rnf IPSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf iPAddressVersion
      `Prelude.seq` Prelude.rnf addresses
