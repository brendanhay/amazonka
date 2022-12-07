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
-- Module      : Amazonka.CustomerProfiles.Types.ZendeskSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ZendeskSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when using Zendesk as a flow source.
--
-- /See:/ 'newZendeskSourceProperties' smart constructor.
data ZendeskSourceProperties = ZendeskSourceProperties'
  { -- | The object specified in the Zendesk flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZendeskSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'zendeskSourceProperties_object' - The object specified in the Zendesk flow source.
newZendeskSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  ZendeskSourceProperties
newZendeskSourceProperties pObject_ =
  ZendeskSourceProperties' {object' = pObject_}

-- | The object specified in the Zendesk flow source.
zendeskSourceProperties_object :: Lens.Lens' ZendeskSourceProperties Prelude.Text
zendeskSourceProperties_object = Lens.lens (\ZendeskSourceProperties' {object'} -> object') (\s@ZendeskSourceProperties' {} a -> s {object' = a} :: ZendeskSourceProperties)

instance Prelude.Hashable ZendeskSourceProperties where
  hashWithSalt _salt ZendeskSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData ZendeskSourceProperties where
  rnf ZendeskSourceProperties' {..} =
    Prelude.rnf object'

instance Data.ToJSON ZendeskSourceProperties where
  toJSON ZendeskSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Object" Data..= object')]
      )
