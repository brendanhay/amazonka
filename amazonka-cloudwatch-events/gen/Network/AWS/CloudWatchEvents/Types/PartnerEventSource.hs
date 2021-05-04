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
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A partner event source is created by an SaaS partner. If a customer
-- creates a partner event bus that matches this event source, that AWS
-- account can receive events from the partner\'s applications or services.
--
-- /See:/ 'newPartnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { -- | The ARN of the partner event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the partner event source.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the partner event source.
partnerEventSource_arn :: Lens.Lens' PartnerEventSource (Prelude.Maybe Prelude.Text)
partnerEventSource_arn = Lens.lens (\PartnerEventSource' {arn} -> arn) (\s@PartnerEventSource' {} a -> s {arn = a} :: PartnerEventSource)

-- | The name of the partner event source.
partnerEventSource_name :: Lens.Lens' PartnerEventSource (Prelude.Maybe Prelude.Text)
partnerEventSource_name = Lens.lens (\PartnerEventSource' {name} -> name) (\s@PartnerEventSource' {} a -> s {name = a} :: PartnerEventSource)

instance Prelude.FromJSON PartnerEventSource where
  parseJSON =
    Prelude.withObject
      "PartnerEventSource"
      ( \x ->
          PartnerEventSource'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable PartnerEventSource

instance Prelude.NFData PartnerEventSource
