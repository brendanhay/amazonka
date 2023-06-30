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
-- Module      : Amazonka.CognitoIdentityProvider.Types.ResourceServerScopeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ResourceServerScopeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource server scope.
--
-- /See:/ 'newResourceServerScopeType' smart constructor.
data ResourceServerScopeType = ResourceServerScopeType'
  { -- | The name of the scope.
    scopeName :: Prelude.Text,
    -- | A description of the scope.
    scopeDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'scopeDescription'
  Prelude.Text ->
  ResourceServerScopeType
newResourceServerScopeType
  pScopeName_
  pScopeDescription_ =
    ResourceServerScopeType'
      { scopeName = pScopeName_,
        scopeDescription = pScopeDescription_
      }

-- | The name of the scope.
resourceServerScopeType_scopeName :: Lens.Lens' ResourceServerScopeType Prelude.Text
resourceServerScopeType_scopeName = Lens.lens (\ResourceServerScopeType' {scopeName} -> scopeName) (\s@ResourceServerScopeType' {} a -> s {scopeName = a} :: ResourceServerScopeType)

-- | A description of the scope.
resourceServerScopeType_scopeDescription :: Lens.Lens' ResourceServerScopeType Prelude.Text
resourceServerScopeType_scopeDescription = Lens.lens (\ResourceServerScopeType' {scopeDescription} -> scopeDescription) (\s@ResourceServerScopeType' {} a -> s {scopeDescription = a} :: ResourceServerScopeType)

instance Data.FromJSON ResourceServerScopeType where
  parseJSON =
    Data.withObject
      "ResourceServerScopeType"
      ( \x ->
          ResourceServerScopeType'
            Prelude.<$> (x Data..: "ScopeName")
            Prelude.<*> (x Data..: "ScopeDescription")
      )

instance Prelude.Hashable ResourceServerScopeType where
  hashWithSalt _salt ResourceServerScopeType' {..} =
    _salt
      `Prelude.hashWithSalt` scopeName
      `Prelude.hashWithSalt` scopeDescription

instance Prelude.NFData ResourceServerScopeType where
  rnf ResourceServerScopeType' {..} =
    Prelude.rnf scopeName
      `Prelude.seq` Prelude.rnf scopeDescription

instance Data.ToJSON ResourceServerScopeType where
  toJSON ResourceServerScopeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ScopeName" Data..= scopeName),
            Prelude.Just
              ("ScopeDescription" Data..= scopeDescription)
          ]
      )
