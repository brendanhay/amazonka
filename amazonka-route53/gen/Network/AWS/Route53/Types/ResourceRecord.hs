{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53.Types.ResourceRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceRecord where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

-- | Information specific to the resource record.
--
-- If you\'re creating an alias resource record set, omit @ResourceRecord@.
--
-- /See:/ 'newResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The current or new DNS record value, not to exceed 4,000 characters. In
    -- the case of a @DELETE@ action, if the current value does not match the
    -- actual value, an error is returned. For descriptions about how to format
    -- @Value@ for different record types, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- You can specify more than one value for all record types except @CNAME@
    -- and @SOA@.
    --
    -- If you\'re creating an alias resource record set, omit @Value@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceRecord_value' - The current or new DNS record value, not to exceed 4,000 characters. In
-- the case of a @DELETE@ action, if the current value does not match the
-- actual value, an error is returned. For descriptions about how to format
-- @Value@ for different record types, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can specify more than one value for all record types except @CNAME@
-- and @SOA@.
--
-- If you\'re creating an alias resource record set, omit @Value@.
newResourceRecord ::
  -- | 'value'
  Prelude.Text ->
  ResourceRecord
newResourceRecord pValue_ =
  ResourceRecord' {value = pValue_}

-- | The current or new DNS record value, not to exceed 4,000 characters. In
-- the case of a @DELETE@ action, if the current value does not match the
-- actual value, an error is returned. For descriptions about how to format
-- @Value@ for different record types, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can specify more than one value for all record types except @CNAME@
-- and @SOA@.
--
-- If you\'re creating an alias resource record set, omit @Value@.
resourceRecord_value :: Lens.Lens' ResourceRecord Prelude.Text
resourceRecord_value = Lens.lens (\ResourceRecord' {value} -> value) (\s@ResourceRecord' {} a -> s {value = a} :: ResourceRecord)

instance Prelude.FromXML ResourceRecord where
  parseXML x =
    ResourceRecord' Prelude.<$> (x Prelude..@ "Value")

instance Prelude.Hashable ResourceRecord

instance Prelude.NFData ResourceRecord

instance Prelude.ToXML ResourceRecord where
  toXML ResourceRecord' {..} =
    Prelude.mconcat ["Value" Prelude.@= value]
