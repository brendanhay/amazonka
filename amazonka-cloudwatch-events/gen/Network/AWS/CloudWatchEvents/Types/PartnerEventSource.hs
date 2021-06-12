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
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A partner event source is created by an SaaS partner. If a customer
-- creates a partner event bus that matches this event source, that AWS
-- account can receive events from the partner\'s applications or services.
--
-- /See:/ 'newPartnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { -- | The ARN of the partner event source.
    arn :: Core.Maybe Core.Text,
    -- | The name of the partner event source.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartnerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'partnerEventSource_arn' - The ARN of the partner event source.
--
-- 'name', 'partnerEventSource_name' - The name of the partner event source.
newPartnerEventSource ::
  PartnerEventSource
newPartnerEventSource =
  PartnerEventSource'
    { arn = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the partner event source.
partnerEventSource_arn :: Lens.Lens' PartnerEventSource (Core.Maybe Core.Text)
partnerEventSource_arn = Lens.lens (\PartnerEventSource' {arn} -> arn) (\s@PartnerEventSource' {} a -> s {arn = a} :: PartnerEventSource)

-- | The name of the partner event source.
partnerEventSource_name :: Lens.Lens' PartnerEventSource (Core.Maybe Core.Text)
partnerEventSource_name = Lens.lens (\PartnerEventSource' {name} -> name) (\s@PartnerEventSource' {} a -> s {name = a} :: PartnerEventSource)

instance Core.FromJSON PartnerEventSource where
  parseJSON =
    Core.withObject
      "PartnerEventSource"
      ( \x ->
          PartnerEventSource'
            Core.<$> (x Core..:? "Arn") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable PartnerEventSource

instance Core.NFData PartnerEventSource
