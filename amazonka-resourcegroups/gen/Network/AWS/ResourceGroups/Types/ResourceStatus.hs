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
-- Module      : Network.AWS.ResourceGroups.Types.ResourceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.ResourceStatusValue

-- | A structure that identifies the current group membership status for a
-- resource. Adding a resource to a resource group is performed
-- asynchronously as a background task. A @PENDING@ status indicates, for
-- this resource, that the process isn\'t completed yet.
--
-- /See:/ 'newResourceStatus' smart constructor.
data ResourceStatus = ResourceStatus'
  { -- | The current status.
    name :: Prelude.Maybe ResourceStatusValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceStatus_name' - The current status.
newResourceStatus ::
  ResourceStatus
newResourceStatus =
  ResourceStatus' {name = Prelude.Nothing}

-- | The current status.
resourceStatus_name :: Lens.Lens' ResourceStatus (Prelude.Maybe ResourceStatusValue)
resourceStatus_name = Lens.lens (\ResourceStatus' {name} -> name) (\s@ResourceStatus' {} a -> s {name = a} :: ResourceStatus)

instance Prelude.FromJSON ResourceStatus where
  parseJSON =
    Prelude.withObject
      "ResourceStatus"
      ( \x ->
          ResourceStatus' Prelude.<$> (x Prelude..:? "Name")
      )

instance Prelude.Hashable ResourceStatus

instance Prelude.NFData ResourceStatus
