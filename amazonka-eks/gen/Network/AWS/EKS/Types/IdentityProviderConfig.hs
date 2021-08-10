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
-- Module      : Network.AWS.EKS.Types.IdentityProviderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.IdentityProviderConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an identity provider configuration.
--
-- /See:/ 'newIdentityProviderConfig' smart constructor.
data IdentityProviderConfig = IdentityProviderConfig'
  { -- | The type of the identity provider configuration.
    type' :: Prelude.Text,
    -- | The name of the identity provider configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'identityProviderConfig_type' - The type of the identity provider configuration.
--
-- 'name', 'identityProviderConfig_name' - The name of the identity provider configuration.
newIdentityProviderConfig ::
  -- | 'type''
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  IdentityProviderConfig
newIdentityProviderConfig pType_ pName_ =
  IdentityProviderConfig'
    { type' = pType_,
      name = pName_
    }

-- | The type of the identity provider configuration.
identityProviderConfig_type :: Lens.Lens' IdentityProviderConfig Prelude.Text
identityProviderConfig_type = Lens.lens (\IdentityProviderConfig' {type'} -> type') (\s@IdentityProviderConfig' {} a -> s {type' = a} :: IdentityProviderConfig)

-- | The name of the identity provider configuration.
identityProviderConfig_name :: Lens.Lens' IdentityProviderConfig Prelude.Text
identityProviderConfig_name = Lens.lens (\IdentityProviderConfig' {name} -> name) (\s@IdentityProviderConfig' {} a -> s {name = a} :: IdentityProviderConfig)

instance Core.FromJSON IdentityProviderConfig where
  parseJSON =
    Core.withObject
      "IdentityProviderConfig"
      ( \x ->
          IdentityProviderConfig'
            Prelude.<$> (x Core..: "type") Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable IdentityProviderConfig

instance Prelude.NFData IdentityProviderConfig

instance Core.ToJSON IdentityProviderConfig where
  toJSON IdentityProviderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Core..= type'),
            Prelude.Just ("name" Core..= name)
          ]
      )
