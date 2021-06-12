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

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

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
    key :: Core.Maybe Core.Text,
    -- | This value of the attribute.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the attribute.
--
-- The following attribute is supported.
--
-- -   @elb.http.desyncmitigationmode@ - Determines how the load balancer
--     handles requests that might pose a security risk to your
--     application. The possible values are @monitor@, @defensive@, and
--     @strictest@. The default is @defensive@.
additionalAttribute_key :: Lens.Lens' AdditionalAttribute (Core.Maybe Core.Text)
additionalAttribute_key = Lens.lens (\AdditionalAttribute' {key} -> key) (\s@AdditionalAttribute' {} a -> s {key = a} :: AdditionalAttribute)

-- | This value of the attribute.
additionalAttribute_value :: Lens.Lens' AdditionalAttribute (Core.Maybe Core.Text)
additionalAttribute_value = Lens.lens (\AdditionalAttribute' {value} -> value) (\s@AdditionalAttribute' {} a -> s {value = a} :: AdditionalAttribute)

instance Core.FromXML AdditionalAttribute where
  parseXML x =
    AdditionalAttribute'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")

instance Core.Hashable AdditionalAttribute

instance Core.NFData AdditionalAttribute

instance Core.ToQuery AdditionalAttribute where
  toQuery AdditionalAttribute' {..} =
    Core.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
