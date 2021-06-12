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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A resource server scope.
--
-- /See:/ 'newResourceServerScopeType' smart constructor.
data ResourceServerScopeType = ResourceServerScopeType'
  { -- | The name of the scope.
    scopeName :: Core.Text,
    -- | A description of the scope.
    scopeDescription :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceServerScopeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopeName', 'resourceServerScopeType_scopeName' - The name of the scope.
--
-- 'scopeDescription', 'resourceServerScopeType_scopeDescription' - A description of the scope.
newResourceServerScopeType ::
  -- | 'scopeName'
  Core.Text ->
  -- | 'scopeDescription'
  Core.Text ->
  ResourceServerScopeType
newResourceServerScopeType
  pScopeName_
  pScopeDescription_ =
    ResourceServerScopeType'
      { scopeName = pScopeName_,
        scopeDescription = pScopeDescription_
      }

-- | The name of the scope.
resourceServerScopeType_scopeName :: Lens.Lens' ResourceServerScopeType Core.Text
resourceServerScopeType_scopeName = Lens.lens (\ResourceServerScopeType' {scopeName} -> scopeName) (\s@ResourceServerScopeType' {} a -> s {scopeName = a} :: ResourceServerScopeType)

-- | A description of the scope.
resourceServerScopeType_scopeDescription :: Lens.Lens' ResourceServerScopeType Core.Text
resourceServerScopeType_scopeDescription = Lens.lens (\ResourceServerScopeType' {scopeDescription} -> scopeDescription) (\s@ResourceServerScopeType' {} a -> s {scopeDescription = a} :: ResourceServerScopeType)

instance Core.FromJSON ResourceServerScopeType where
  parseJSON =
    Core.withObject
      "ResourceServerScopeType"
      ( \x ->
          ResourceServerScopeType'
            Core.<$> (x Core..: "ScopeName")
            Core.<*> (x Core..: "ScopeDescription")
      )

instance Core.Hashable ResourceServerScopeType

instance Core.NFData ResourceServerScopeType

instance Core.ToJSON ResourceServerScopeType where
  toJSON ResourceServerScopeType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ScopeName" Core..= scopeName),
            Core.Just
              ("ScopeDescription" Core..= scopeDescription)
          ]
      )
