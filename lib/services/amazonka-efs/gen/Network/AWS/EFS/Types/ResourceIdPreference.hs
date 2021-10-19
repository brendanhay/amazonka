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
-- Module      : Network.AWS.EFS.Types.ResourceIdPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.ResourceIdPreference where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.Resource
import Network.AWS.EFS.Types.ResourceIdType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the resource type and its ID preference for the user\'s Amazon
-- Web Services account, in the current Amazon Web Services Region.
--
-- /See:/ 'newResourceIdPreference' smart constructor.
data ResourceIdPreference = ResourceIdPreference'
  { -- | Identifies the Amazon EFS resources to which the ID preference setting
    -- applies, @FILE_SYSTEM@ and @MOUNT_TARGET@.
    resources :: Prelude.Maybe [Resource],
    -- | Identifies the EFS resource ID preference, either @LONG_ID@ (17
    -- characters) or @SHORT_ID@ (8 characters).
    resourceIdType :: Prelude.Maybe ResourceIdType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIdPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'resourceIdPreference_resources' - Identifies the Amazon EFS resources to which the ID preference setting
-- applies, @FILE_SYSTEM@ and @MOUNT_TARGET@.
--
-- 'resourceIdType', 'resourceIdPreference_resourceIdType' - Identifies the EFS resource ID preference, either @LONG_ID@ (17
-- characters) or @SHORT_ID@ (8 characters).
newResourceIdPreference ::
  ResourceIdPreference
newResourceIdPreference =
  ResourceIdPreference'
    { resources = Prelude.Nothing,
      resourceIdType = Prelude.Nothing
    }

-- | Identifies the Amazon EFS resources to which the ID preference setting
-- applies, @FILE_SYSTEM@ and @MOUNT_TARGET@.
resourceIdPreference_resources :: Lens.Lens' ResourceIdPreference (Prelude.Maybe [Resource])
resourceIdPreference_resources = Lens.lens (\ResourceIdPreference' {resources} -> resources) (\s@ResourceIdPreference' {} a -> s {resources = a} :: ResourceIdPreference) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the EFS resource ID preference, either @LONG_ID@ (17
-- characters) or @SHORT_ID@ (8 characters).
resourceIdPreference_resourceIdType :: Lens.Lens' ResourceIdPreference (Prelude.Maybe ResourceIdType)
resourceIdPreference_resourceIdType = Lens.lens (\ResourceIdPreference' {resourceIdType} -> resourceIdType) (\s@ResourceIdPreference' {} a -> s {resourceIdType = a} :: ResourceIdPreference)

instance Core.FromJSON ResourceIdPreference where
  parseJSON =
    Core.withObject
      "ResourceIdPreference"
      ( \x ->
          ResourceIdPreference'
            Prelude.<$> (x Core..:? "Resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceIdType")
      )

instance Prelude.Hashable ResourceIdPreference

instance Prelude.NFData ResourceIdPreference
