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
-- Module      : Network.AWS.CloudFormation.Types.Change
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Change where

import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.ResourceChange
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @Change@ structure describes the changes AWS CloudFormation will
-- perform if you execute the change set.
--
-- /See:/ 'newChange' smart constructor.
data Change = Change'
  { -- | A @ResourceChange@ structure that describes the resource and action that
    -- AWS CloudFormation will perform.
    resourceChange :: Core.Maybe ResourceChange,
    -- | The type of entity that AWS CloudFormation changes. Currently, the only
    -- entity type is @Resource@.
    type' :: Core.Maybe ChangeType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Change' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceChange', 'change_resourceChange' - A @ResourceChange@ structure that describes the resource and action that
-- AWS CloudFormation will perform.
--
-- 'type'', 'change_type' - The type of entity that AWS CloudFormation changes. Currently, the only
-- entity type is @Resource@.
newChange ::
  Change
newChange =
  Change'
    { resourceChange = Core.Nothing,
      type' = Core.Nothing
    }

-- | A @ResourceChange@ structure that describes the resource and action that
-- AWS CloudFormation will perform.
change_resourceChange :: Lens.Lens' Change (Core.Maybe ResourceChange)
change_resourceChange = Lens.lens (\Change' {resourceChange} -> resourceChange) (\s@Change' {} a -> s {resourceChange = a} :: Change)

-- | The type of entity that AWS CloudFormation changes. Currently, the only
-- entity type is @Resource@.
change_type :: Lens.Lens' Change (Core.Maybe ChangeType)
change_type = Lens.lens (\Change' {type'} -> type') (\s@Change' {} a -> s {type' = a} :: Change)

instance Core.FromXML Change where
  parseXML x =
    Change'
      Core.<$> (x Core..@? "ResourceChange")
      Core.<*> (x Core..@? "Type")

instance Core.Hashable Change

instance Core.NFData Change
