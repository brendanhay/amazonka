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
-- Module      : Network.AWS.Batch.Types.NodePropertyOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodePropertyOverride where

import Network.AWS.Batch.Types.ContainerOverrides
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Object representing any node overrides to a job definition that\'s used
-- in a SubmitJob API operation.
--
-- /See:/ 'newNodePropertyOverride' smart constructor.
data NodePropertyOverride = NodePropertyOverride'
  { -- | The overrides that should be sent to a node range.
    containerOverrides :: Prelude.Maybe ContainerOverrides,
    -- | The range of nodes, using node index values, that\'s used to override. A
    -- range of @0:3@ indicates nodes with index values of @0@ through @3@. If
    -- the starting range value is omitted (@:n@), then @0@ is used to start
    -- the range. If the ending range value is omitted (@n:@), then the highest
    -- possible node index is used to end the range.
    targetNodes :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NodePropertyOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerOverrides', 'nodePropertyOverride_containerOverrides' - The overrides that should be sent to a node range.
--
-- 'targetNodes', 'nodePropertyOverride_targetNodes' - The range of nodes, using node index values, that\'s used to override. A
-- range of @0:3@ indicates nodes with index values of @0@ through @3@. If
-- the starting range value is omitted (@:n@), then @0@ is used to start
-- the range. If the ending range value is omitted (@n:@), then the highest
-- possible node index is used to end the range.
newNodePropertyOverride ::
  -- | 'targetNodes'
  Prelude.Text ->
  NodePropertyOverride
newNodePropertyOverride pTargetNodes_ =
  NodePropertyOverride'
    { containerOverrides =
        Prelude.Nothing,
      targetNodes = pTargetNodes_
    }

-- | The overrides that should be sent to a node range.
nodePropertyOverride_containerOverrides :: Lens.Lens' NodePropertyOverride (Prelude.Maybe ContainerOverrides)
nodePropertyOverride_containerOverrides = Lens.lens (\NodePropertyOverride' {containerOverrides} -> containerOverrides) (\s@NodePropertyOverride' {} a -> s {containerOverrides = a} :: NodePropertyOverride)

-- | The range of nodes, using node index values, that\'s used to override. A
-- range of @0:3@ indicates nodes with index values of @0@ through @3@. If
-- the starting range value is omitted (@:n@), then @0@ is used to start
-- the range. If the ending range value is omitted (@n:@), then the highest
-- possible node index is used to end the range.
nodePropertyOverride_targetNodes :: Lens.Lens' NodePropertyOverride Prelude.Text
nodePropertyOverride_targetNodes = Lens.lens (\NodePropertyOverride' {targetNodes} -> targetNodes) (\s@NodePropertyOverride' {} a -> s {targetNodes = a} :: NodePropertyOverride)

instance Prelude.Hashable NodePropertyOverride

instance Prelude.NFData NodePropertyOverride

instance Prelude.ToJSON NodePropertyOverride where
  toJSON NodePropertyOverride' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("containerOverrides" Prelude..=)
              Prelude.<$> containerOverrides,
            Prelude.Just ("targetNodes" Prelude..= targetNodes)
          ]
      )
