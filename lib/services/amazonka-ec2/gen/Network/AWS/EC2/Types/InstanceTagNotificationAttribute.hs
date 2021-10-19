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
import qualified Network.AWS.Prelude as Prelude

-- | Describes the registered tag keys for the current Region.
--
-- /See:/ 'newInstanceTagNotificationAttribute' smart constructor.
data InstanceTagNotificationAttribute = InstanceTagNotificationAttribute'
  { -- | Indicates wheter all tag keys in the current Region are registered to
    -- appear in scheduled event notifications. @true@ indicates that all tag
    -- keys in the current Region are registered.
    includeAllTagsOfInstance :: Prelude.Maybe Prelude.Bool,
    -- | The registered tag keys.
    instanceTagKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTagNotificationAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeAllTagsOfInstance', 'instanceTagNotificationAttribute_includeAllTagsOfInstance' - Indicates wheter all tag keys in the current Region are registered to
-- appear in scheduled event notifications. @true@ indicates that all tag
-- keys in the current Region are registered.
--
-- 'instanceTagKeys', 'instanceTagNotificationAttribute_instanceTagKeys' - The registered tag keys.
newInstanceTagNotificationAttribute ::
  InstanceTagNotificationAttribute
newInstanceTagNotificationAttribute =
  InstanceTagNotificationAttribute'
    { includeAllTagsOfInstance =
        Prelude.Nothing,
      instanceTagKeys = Prelude.Nothing
    }

-- | Indicates wheter all tag keys in the current Region are registered to
-- appear in scheduled event notifications. @true@ indicates that all tag
-- keys in the current Region are registered.
instanceTagNotificationAttribute_includeAllTagsOfInstance :: Lens.Lens' InstanceTagNotificationAttribute (Prelude.Maybe Prelude.Bool)
instanceTagNotificationAttribute_includeAllTagsOfInstance = Lens.lens (\InstanceTagNotificationAttribute' {includeAllTagsOfInstance} -> includeAllTagsOfInstance) (\s@InstanceTagNotificationAttribute' {} a -> s {includeAllTagsOfInstance = a} :: InstanceTagNotificationAttribute)

-- | The registered tag keys.
instanceTagNotificationAttribute_instanceTagKeys :: Lens.Lens' InstanceTagNotificationAttribute (Prelude.Maybe [Prelude.Text])
instanceTagNotificationAttribute_instanceTagKeys = Lens.lens (\InstanceTagNotificationAttribute' {instanceTagKeys} -> instanceTagKeys) (\s@InstanceTagNotificationAttribute' {} a -> s {instanceTagKeys = a} :: InstanceTagNotificationAttribute) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromXML
    InstanceTagNotificationAttribute
  where
  parseXML x =
    InstanceTagNotificationAttribute'
      Prelude.<$> (x Core..@? "includeAllTagsOfInstance")
      Prelude.<*> ( x Core..@? "instanceTagKeySet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    InstanceTagNotificationAttribute

instance
  Prelude.NFData
    InstanceTagNotificationAttribute
