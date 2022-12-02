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
-- Module      : Amazonka.AppFlow.Types.ZendeskMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ZendeskMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Zendesk.
--
-- /See:/ 'newZendeskMetadata' smart constructor.
data ZendeskMetadata = ZendeskMetadata'
  { -- | The desired authorization scope for the Zendesk account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZendeskMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthScopes', 'zendeskMetadata_oAuthScopes' - The desired authorization scope for the Zendesk account.
newZendeskMetadata ::
  ZendeskMetadata
newZendeskMetadata =
  ZendeskMetadata' {oAuthScopes = Prelude.Nothing}

-- | The desired authorization scope for the Zendesk account.
zendeskMetadata_oAuthScopes :: Lens.Lens' ZendeskMetadata (Prelude.Maybe [Prelude.Text])
zendeskMetadata_oAuthScopes = Lens.lens (\ZendeskMetadata' {oAuthScopes} -> oAuthScopes) (\s@ZendeskMetadata' {} a -> s {oAuthScopes = a} :: ZendeskMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ZendeskMetadata where
  parseJSON =
    Data.withObject
      "ZendeskMetadata"
      ( \x ->
          ZendeskMetadata'
            Prelude.<$> (x Data..:? "oAuthScopes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ZendeskMetadata where
  hashWithSalt _salt ZendeskMetadata' {..} =
    _salt `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData ZendeskMetadata where
  rnf ZendeskMetadata' {..} = Prelude.rnf oAuthScopes
