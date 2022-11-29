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
-- Module      : Amazonka.ServiceCatalog.Types.OrganizationNode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.OrganizationNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.OrganizationNodeType

-- | Information about the organization node.
--
-- /See:/ 'newOrganizationNode' smart constructor.
data OrganizationNode = OrganizationNode'
  { -- | The organization node type.
    type' :: Prelude.Maybe OrganizationNodeType,
    -- | The identifier of the organization node.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'organizationNode_type' - The organization node type.
--
-- 'value', 'organizationNode_value' - The identifier of the organization node.
newOrganizationNode ::
  OrganizationNode
newOrganizationNode =
  OrganizationNode'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The organization node type.
organizationNode_type :: Lens.Lens' OrganizationNode (Prelude.Maybe OrganizationNodeType)
organizationNode_type = Lens.lens (\OrganizationNode' {type'} -> type') (\s@OrganizationNode' {} a -> s {type' = a} :: OrganizationNode)

-- | The identifier of the organization node.
organizationNode_value :: Lens.Lens' OrganizationNode (Prelude.Maybe Prelude.Text)
organizationNode_value = Lens.lens (\OrganizationNode' {value} -> value) (\s@OrganizationNode' {} a -> s {value = a} :: OrganizationNode)

instance Core.FromJSON OrganizationNode where
  parseJSON =
    Core.withObject
      "OrganizationNode"
      ( \x ->
          OrganizationNode'
            Prelude.<$> (x Core..:? "Type") Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable OrganizationNode where
  hashWithSalt _salt OrganizationNode' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData OrganizationNode where
  rnf OrganizationNode' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Core.ToJSON OrganizationNode where
  toJSON OrganizationNode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("Value" Core..=) Prelude.<$> value
          ]
      )
