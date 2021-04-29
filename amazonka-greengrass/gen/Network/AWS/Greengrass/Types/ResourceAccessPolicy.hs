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
-- Module      : Network.AWS.Greengrass.Types.ResourceAccessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceAccessPolicy where

import Network.AWS.Greengrass.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A policy used by the function to access a resource.
--
-- /See:/ 'newResourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { -- | The permissions that the Lambda function has to the resource. Can be one
    -- of \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
    permission :: Prelude.Maybe Permission,
    -- | The ID of the resource. (This ID is assigned to the resource when you
    -- create the resource definiton.)
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 'resourceAccessPolicy_permission' - The permissions that the Lambda function has to the resource. Can be one
-- of \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
--
-- 'resourceId', 'resourceAccessPolicy_resourceId' - The ID of the resource. (This ID is assigned to the resource when you
-- create the resource definiton.)
newResourceAccessPolicy ::
  -- | 'resourceId'
  Prelude.Text ->
  ResourceAccessPolicy
newResourceAccessPolicy pResourceId_ =
  ResourceAccessPolicy'
    { permission = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The permissions that the Lambda function has to the resource. Can be one
-- of \'\'rw\'\' (read\/write) or \'\'ro\'\' (read-only).
resourceAccessPolicy_permission :: Lens.Lens' ResourceAccessPolicy (Prelude.Maybe Permission)
resourceAccessPolicy_permission = Lens.lens (\ResourceAccessPolicy' {permission} -> permission) (\s@ResourceAccessPolicy' {} a -> s {permission = a} :: ResourceAccessPolicy)

-- | The ID of the resource. (This ID is assigned to the resource when you
-- create the resource definiton.)
resourceAccessPolicy_resourceId :: Lens.Lens' ResourceAccessPolicy Prelude.Text
resourceAccessPolicy_resourceId = Lens.lens (\ResourceAccessPolicy' {resourceId} -> resourceId) (\s@ResourceAccessPolicy' {} a -> s {resourceId = a} :: ResourceAccessPolicy)

instance Prelude.FromJSON ResourceAccessPolicy where
  parseJSON =
    Prelude.withObject
      "ResourceAccessPolicy"
      ( \x ->
          ResourceAccessPolicy'
            Prelude.<$> (x Prelude..:? "Permission")
            Prelude.<*> (x Prelude..: "ResourceId")
      )

instance Prelude.Hashable ResourceAccessPolicy

instance Prelude.NFData ResourceAccessPolicy

instance Prelude.ToJSON ResourceAccessPolicy where
  toJSON ResourceAccessPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Permission" Prelude..=) Prelude.<$> permission,
            Prelude.Just ("ResourceId" Prelude..= resourceId)
          ]
      )
