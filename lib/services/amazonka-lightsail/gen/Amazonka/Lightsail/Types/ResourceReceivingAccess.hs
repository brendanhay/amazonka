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
-- Module      : Amazonka.Lightsail.Types.ResourceReceivingAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ResourceReceivingAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lightsail instance that has access to a Lightsail
-- bucket.
--
-- /See:/ 'newResourceReceivingAccess' smart constructor.
data ResourceReceivingAccess = ResourceReceivingAccess'
  { -- | The name of the Lightsail instance.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (for example, @Instance@).
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceReceivingAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceReceivingAccess_name' - The name of the Lightsail instance.
--
-- 'resourceType', 'resourceReceivingAccess_resourceType' - The Lightsail resource type (for example, @Instance@).
newResourceReceivingAccess ::
  ResourceReceivingAccess
newResourceReceivingAccess =
  ResourceReceivingAccess'
    { name = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The name of the Lightsail instance.
resourceReceivingAccess_name :: Lens.Lens' ResourceReceivingAccess (Prelude.Maybe Prelude.Text)
resourceReceivingAccess_name = Lens.lens (\ResourceReceivingAccess' {name} -> name) (\s@ResourceReceivingAccess' {} a -> s {name = a} :: ResourceReceivingAccess)

-- | The Lightsail resource type (for example, @Instance@).
resourceReceivingAccess_resourceType :: Lens.Lens' ResourceReceivingAccess (Prelude.Maybe Prelude.Text)
resourceReceivingAccess_resourceType = Lens.lens (\ResourceReceivingAccess' {resourceType} -> resourceType) (\s@ResourceReceivingAccess' {} a -> s {resourceType = a} :: ResourceReceivingAccess)

instance Data.FromJSON ResourceReceivingAccess where
  parseJSON =
    Data.withObject
      "ResourceReceivingAccess"
      ( \x ->
          ResourceReceivingAccess'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable ResourceReceivingAccess where
  hashWithSalt _salt ResourceReceivingAccess' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceReceivingAccess where
  rnf ResourceReceivingAccess' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
