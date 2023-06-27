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
-- Module      : Amazonka.ServiceCatalog.Types.EngineWorkflowResourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.EngineWorkflowResourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.UniqueTagResourceIdentifier

-- | The ID for the provisioned product resources that are part of a resource
-- group.
--
-- /See:/ 'newEngineWorkflowResourceIdentifier' smart constructor.
data EngineWorkflowResourceIdentifier = EngineWorkflowResourceIdentifier'
  { -- | The unique key-value pair for a tag that identifies provisioned product
    -- resources.
    uniqueTag :: Prelude.Maybe UniqueTagResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineWorkflowResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uniqueTag', 'engineWorkflowResourceIdentifier_uniqueTag' - The unique key-value pair for a tag that identifies provisioned product
-- resources.
newEngineWorkflowResourceIdentifier ::
  EngineWorkflowResourceIdentifier
newEngineWorkflowResourceIdentifier =
  EngineWorkflowResourceIdentifier'
    { uniqueTag =
        Prelude.Nothing
    }

-- | The unique key-value pair for a tag that identifies provisioned product
-- resources.
engineWorkflowResourceIdentifier_uniqueTag :: Lens.Lens' EngineWorkflowResourceIdentifier (Prelude.Maybe UniqueTagResourceIdentifier)
engineWorkflowResourceIdentifier_uniqueTag = Lens.lens (\EngineWorkflowResourceIdentifier' {uniqueTag} -> uniqueTag) (\s@EngineWorkflowResourceIdentifier' {} a -> s {uniqueTag = a} :: EngineWorkflowResourceIdentifier)

instance
  Prelude.Hashable
    EngineWorkflowResourceIdentifier
  where
  hashWithSalt
    _salt
    EngineWorkflowResourceIdentifier' {..} =
      _salt `Prelude.hashWithSalt` uniqueTag

instance
  Prelude.NFData
    EngineWorkflowResourceIdentifier
  where
  rnf EngineWorkflowResourceIdentifier' {..} =
    Prelude.rnf uniqueTag

instance Data.ToJSON EngineWorkflowResourceIdentifier where
  toJSON EngineWorkflowResourceIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [("UniqueTag" Data..=) Prelude.<$> uniqueTag]
      )
