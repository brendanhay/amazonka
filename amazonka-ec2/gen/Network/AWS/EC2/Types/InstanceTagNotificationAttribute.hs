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
-- Module      : Network.AWS.EC2.Types.InstanceTagNotificationAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTagNotificationAttribute where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the registered tag keys for the current Region.
--
-- /See:/ 'newInstanceTagNotificationAttribute' smart constructor.
data InstanceTagNotificationAttribute = InstanceTagNotificationAttribute'
  { -- | The registered tag keys.
    instanceTagKeys :: Core.Maybe [Core.Text],
    -- | Indicates wheter all tag keys in the current Region are registered to
    -- appear in scheduled event notifications. @true@ indicates that all tag
    -- keys in the current Region are registered.
    includeAllTagsOfInstance :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceTagNotificationAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagKeys', 'instanceTagNotificationAttribute_instanceTagKeys' - The registered tag keys.
--
-- 'includeAllTagsOfInstance', 'instanceTagNotificationAttribute_includeAllTagsOfInstance' - Indicates wheter all tag keys in the current Region are registered to
-- appear in scheduled event notifications. @true@ indicates that all tag
-- keys in the current Region are registered.
newInstanceTagNotificationAttribute ::
  InstanceTagNotificationAttribute
newInstanceTagNotificationAttribute =
  InstanceTagNotificationAttribute'
    { instanceTagKeys =
        Core.Nothing,
      includeAllTagsOfInstance = Core.Nothing
    }

-- | The registered tag keys.
instanceTagNotificationAttribute_instanceTagKeys :: Lens.Lens' InstanceTagNotificationAttribute (Core.Maybe [Core.Text])
instanceTagNotificationAttribute_instanceTagKeys = Lens.lens (\InstanceTagNotificationAttribute' {instanceTagKeys} -> instanceTagKeys) (\s@InstanceTagNotificationAttribute' {} a -> s {instanceTagKeys = a} :: InstanceTagNotificationAttribute) Core.. Lens.mapping Lens._Coerce

-- | Indicates wheter all tag keys in the current Region are registered to
-- appear in scheduled event notifications. @true@ indicates that all tag
-- keys in the current Region are registered.
instanceTagNotificationAttribute_includeAllTagsOfInstance :: Lens.Lens' InstanceTagNotificationAttribute (Core.Maybe Core.Bool)
instanceTagNotificationAttribute_includeAllTagsOfInstance = Lens.lens (\InstanceTagNotificationAttribute' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@InstanceTagNotificationAttribute' {} a -> s {includeAllTagsOfInstance = a} :: InstanceTagNotificationAttribute)

instance
  Core.FromXML
    InstanceTagNotificationAttribute
  where
  parseXML x =
    InstanceTagNotificationAttribute'
      Core.<$> ( x Core..@? "instanceTagKeySet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "includeAllTagsOfInstance")

instance
  Core.Hashable
    InstanceTagNotificationAttribute

instance Core.NFData InstanceTagNotificationAttribute
