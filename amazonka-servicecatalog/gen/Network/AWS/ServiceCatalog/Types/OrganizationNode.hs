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
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNode where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.OrganizationNodeType

-- | Information about the organization node.
--
-- /See:/ 'newOrganizationNode' smart constructor.
data OrganizationNode = OrganizationNode'
  { -- | The identifier of the organization node.
    value :: Prelude.Maybe Prelude.Text,
    -- | The organization node type.
    type' :: Prelude.Maybe OrganizationNodeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'organizationNode_value' - The identifier of the organization node.
--
-- 'type'', 'organizationNode_type' - The organization node type.
newOrganizationNode ::
  OrganizationNode
newOrganizationNode =
  OrganizationNode'
    { value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The identifier of the organization node.
organizationNode_value :: Lens.Lens' OrganizationNode (Prelude.Maybe Prelude.Text)
organizationNode_value = Lens.lens (\OrganizationNode' {value} -> value) (\s@OrganizationNode' {} a -> s {value = a} :: OrganizationNode)

-- | The organization node type.
organizationNode_type :: Lens.Lens' OrganizationNode (Prelude.Maybe OrganizationNodeType)
organizationNode_type = Lens.lens (\OrganizationNode' {type'} -> type') (\s@OrganizationNode' {} a -> s {type' = a} :: OrganizationNode)

instance Prelude.FromJSON OrganizationNode where
  parseJSON =
    Prelude.withObject
      "OrganizationNode"
      ( \x ->
          OrganizationNode'
            Prelude.<$> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable OrganizationNode

instance Prelude.NFData OrganizationNode

instance Prelude.ToJSON OrganizationNode where
  toJSON OrganizationNode' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Value" Prelude..=) Prelude.<$> value,
            ("Type" Prelude..=) Prelude.<$> type'
          ]
      )
