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
-- Module      : Network.AWS.ELB.Types.AdditionalAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AdditionalAttribute where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about additional load balancer attributes.
--
-- /See:/ 'newAdditionalAttribute' smart constructor.
data AdditionalAttribute = AdditionalAttribute'
  { -- | The name of the attribute.
    --
    -- The following attribute is supported.
    --
    -- -   @elb.http.desyncmitigationmode@ - Determines how the load balancer
    --     handles requests that might pose a security risk to your
    --     application. The possible values are @monitor@, @defensive@, and
    --     @strictest@. The default is @defensive@.
    key :: Prelude.Maybe Prelude.Text,
    -- | This value of the attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdditionalAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'additionalAttribute_key' - The name of the attribute.
--
-- The following attribute is supported.
--
-- -   @elb.http.desyncmitigationmode@ - Determines how the load balancer
--     handles requests that might pose a security risk to your
--     application. The possible values are @monitor@, @defensive@, and
--     @strictest@. The default is @defensive@.
--
-- 'value', 'additionalAttribute_value' - This value of the attribute.
newAdditionalAttribute ::
  AdditionalAttribute
newAdditionalAttribute =
  AdditionalAttribute'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the attribute.
--
-- The following attribute is supported.
--
-- -   @elb.http.desyncmitigationmode@ - Determines how the load balancer
--     handles requests that might pose a security risk to your
--     application. The possible values are @monitor@, @defensive@, and
--     @strictest@. The default is @defensive@.
additionalAttribute_key :: Lens.Lens' AdditionalAttribute (Prelude.Maybe Prelude.Text)
additionalAttribute_key = Lens.lens (\AdditionalAttribute' {key} -> key) (\s@AdditionalAttribute' {} a -> s {key = a} :: AdditionalAttribute)

-- | This value of the attribute.
additionalAttribute_value :: Lens.Lens' AdditionalAttribute (Prelude.Maybe Prelude.Text)
additionalAttribute_value = Lens.lens (\AdditionalAttribute' {value} -> value) (\s@AdditionalAttribute' {} a -> s {value = a} :: AdditionalAttribute)

instance Prelude.FromXML AdditionalAttribute where
  parseXML x =
    AdditionalAttribute'
      Prelude.<$> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable AdditionalAttribute

instance Prelude.NFData AdditionalAttribute

instance Prelude.ToQuery AdditionalAttribute where
  toQuery AdditionalAttribute' {..} =
    Prelude.mconcat
      ["Key" Prelude.=: key, "Value" Prelude.=: value]
