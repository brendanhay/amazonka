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
-- Module      : Amazonka.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.PartnerEventSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A partner event source is created by an SaaS partner. If a customer
-- creates a partner event bus that matches this event source, that Amazon
-- Web Services account can receive events from the partner\'s applications
-- or services.
--
-- /See:/ 'newPartnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { -- | The ARN of the partner event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the partner event source.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PartnerEventSource where
  parseJSON =
    Data.withObject
      "PartnerEventSource"
      ( \x ->
          PartnerEventSource'
            Prelude.<$> (x Data..:? "Arn") Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable PartnerEventSource where
  hashWithSalt _salt PartnerEventSource' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name

instance Prelude.NFData PartnerEventSource where
  rnf PartnerEventSource' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name
