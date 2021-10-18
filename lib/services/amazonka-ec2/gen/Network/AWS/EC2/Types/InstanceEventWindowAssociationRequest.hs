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
-- Module      : Network.AWS.EC2.Types.InstanceEventWindowAssociationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceEventWindowAssociationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One or more targets associated with the specified event window. Only one
-- /type/ of target (instance ID, instance tag, or Dedicated Host ID) can
-- be associated with an event window.
--
-- /See:/ 'newInstanceEventWindowAssociationRequest' smart constructor.
data InstanceEventWindowAssociationRequest = InstanceEventWindowAssociationRequest'
  { -- | The IDs of the instances to associate with the event window. If the
    -- instance is on a Dedicated Host, you can\'t specify the Instance ID
    -- parameter; you must use the Dedicated Host ID parameter.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance tags to associate with the event window. Any instances
    -- associated with the tags will be associated with the event window.
    instanceTags :: Prelude.Maybe [Tag],
    -- | The IDs of the Dedicated Hosts to associate with the event window.
    dedicatedHostIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindowAssociationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'instanceEventWindowAssociationRequest_instanceIds' - The IDs of the instances to associate with the event window. If the
-- instance is on a Dedicated Host, you can\'t specify the Instance ID
-- parameter; you must use the Dedicated Host ID parameter.
--
-- 'instanceTags', 'instanceEventWindowAssociationRequest_instanceTags' - The instance tags to associate with the event window. Any instances
-- associated with the tags will be associated with the event window.
--
-- 'dedicatedHostIds', 'instanceEventWindowAssociationRequest_dedicatedHostIds' - The IDs of the Dedicated Hosts to associate with the event window.
newInstanceEventWindowAssociationRequest ::
  InstanceEventWindowAssociationRequest
newInstanceEventWindowAssociationRequest =
  InstanceEventWindowAssociationRequest'
    { instanceIds =
        Prelude.Nothing,
      instanceTags = Prelude.Nothing,
      dedicatedHostIds = Prelude.Nothing
    }

-- | The IDs of the instances to associate with the event window. If the
-- instance is on a Dedicated Host, you can\'t specify the Instance ID
-- parameter; you must use the Dedicated Host ID parameter.
instanceEventWindowAssociationRequest_instanceIds :: Lens.Lens' InstanceEventWindowAssociationRequest (Prelude.Maybe [Prelude.Text])
instanceEventWindowAssociationRequest_instanceIds = Lens.lens (\InstanceEventWindowAssociationRequest' {instanceIds} -> instanceIds) (\s@InstanceEventWindowAssociationRequest' {} a -> s {instanceIds = a} :: InstanceEventWindowAssociationRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The instance tags to associate with the event window. Any instances
-- associated with the tags will be associated with the event window.
instanceEventWindowAssociationRequest_instanceTags :: Lens.Lens' InstanceEventWindowAssociationRequest (Prelude.Maybe [Tag])
instanceEventWindowAssociationRequest_instanceTags = Lens.lens (\InstanceEventWindowAssociationRequest' {instanceTags} -> instanceTags) (\s@InstanceEventWindowAssociationRequest' {} a -> s {instanceTags = a} :: InstanceEventWindowAssociationRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The IDs of the Dedicated Hosts to associate with the event window.
instanceEventWindowAssociationRequest_dedicatedHostIds :: Lens.Lens' InstanceEventWindowAssociationRequest (Prelude.Maybe [Prelude.Text])
instanceEventWindowAssociationRequest_dedicatedHostIds = Lens.lens (\InstanceEventWindowAssociationRequest' {dedicatedHostIds} -> dedicatedHostIds) (\s@InstanceEventWindowAssociationRequest' {} a -> s {dedicatedHostIds = a} :: InstanceEventWindowAssociationRequest) Prelude.. Lens.mapping Lens._Coerce

instance
  Prelude.Hashable
    InstanceEventWindowAssociationRequest

instance
  Prelude.NFData
    InstanceEventWindowAssociationRequest

instance
  Core.ToQuery
    InstanceEventWindowAssociationRequest
  where
  toQuery InstanceEventWindowAssociationRequest' {..} =
    Prelude.mconcat
      [ Core.toQuery
          ( Core.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        Core.toQuery
          ( Core.toQueryList "InstanceTag"
              Prelude.<$> instanceTags
          ),
        Core.toQuery
          ( Core.toQueryList "DedicatedHostId"
              Prelude.<$> dedicatedHostIds
          )
      ]
