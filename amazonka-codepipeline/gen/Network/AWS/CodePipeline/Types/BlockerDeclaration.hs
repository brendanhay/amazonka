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
-- Module      : Network.AWS.CodePipeline.Types.BlockerDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.BlockerDeclaration where

import Network.AWS.CodePipeline.Types.BlockerType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Reserved for future use.
--
-- /See:/ 'newBlockerDeclaration' smart constructor.
data BlockerDeclaration = BlockerDeclaration'
  { -- | Reserved for future use.
    name :: Core.Text,
    -- | Reserved for future use.
    type' :: BlockerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BlockerDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'blockerDeclaration_name' - Reserved for future use.
--
-- 'type'', 'blockerDeclaration_type' - Reserved for future use.
newBlockerDeclaration ::
  -- | 'name'
  Core.Text ->
  -- | 'type''
  BlockerType ->
  BlockerDeclaration
newBlockerDeclaration pName_ pType_ =
  BlockerDeclaration' {name = pName_, type' = pType_}

-- | Reserved for future use.
blockerDeclaration_name :: Lens.Lens' BlockerDeclaration Core.Text
blockerDeclaration_name = Lens.lens (\BlockerDeclaration' {name} -> name) (\s@BlockerDeclaration' {} a -> s {name = a} :: BlockerDeclaration)

-- | Reserved for future use.
blockerDeclaration_type :: Lens.Lens' BlockerDeclaration BlockerType
blockerDeclaration_type = Lens.lens (\BlockerDeclaration' {type'} -> type') (\s@BlockerDeclaration' {} a -> s {type' = a} :: BlockerDeclaration)

instance Core.FromJSON BlockerDeclaration where
  parseJSON =
    Core.withObject
      "BlockerDeclaration"
      ( \x ->
          BlockerDeclaration'
            Core.<$> (x Core..: "name") Core.<*> (x Core..: "type")
      )

instance Core.Hashable BlockerDeclaration

instance Core.NFData BlockerDeclaration

instance Core.ToJSON BlockerDeclaration where
  toJSON BlockerDeclaration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type')
          ]
      )
