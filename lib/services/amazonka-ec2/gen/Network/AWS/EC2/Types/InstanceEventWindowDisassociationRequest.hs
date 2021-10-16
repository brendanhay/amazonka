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
-- Module      : Network.AWS.EC2.Types.InstanceEventWindowDisassociationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceEventWindowDisassociationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The targets to disassociate from the specified event window.
--
-- /See:/ 'newInstanceEventWindowDisassociationRequest' smart constructor.
data InstanceEventWindowDisassociationRequest = InstanceEventWindowDisassociationRequest'
  { -- | The IDs of the instances to disassociate from the event window.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance tags to disassociate from the event window. Any instances
    -- associated with the tags will be disassociated from the event window.
    instanceTags :: Prelude.Maybe [Tag],
    -- | The IDs of the Dedicated Hosts to disassociate from the event window.
    dedicatedHostIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindowDisassociationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'instanceEventWindowDisassociationRequest_instanceIds' - The IDs of the instances to disassociate from the event window.
--
-- 'instanceTags', 'instanceEventWindowDisassociationRequest_instanceTags' - The instance tags to disassociate from the event window. Any instances
-- associated with the tags will be disassociated from the event window.
--
-- 'dedicatedHostIds', 'instanceEventWindowDisassociationRequest_dedicatedHostIds' - The IDs of the Dedicated Hosts to disassociate from the event window.
newInstanceEventWindowDisassociationRequest ::
  InstanceEventWindowDisassociationRequest
newInstanceEventWindowDisassociationRequest =
  InstanceEventWindowDisassociationRequest'
    { instanceIds =
        Prelude.Nothing,
      instanceTags = Prelude.Nothing,
      dedicatedHostIds =
        Prelude.Nothing
    }

-- | The IDs of the instances to disassociate from the event window.
instanceEventWindowDisassociationRequest_instanceIds :: Lens.Lens' InstanceEventWindowDisassociationRequest (Prelude.Maybe [Prelude.Text])
instanceEventWindowDisassociationRequest_instanceIds = Lens.lens (\InstanceEventWindowDisassociationRequest' {instanceIds} -> instanceIds) (\s@InstanceEventWindowDisassociationRequest' {} a -> s {instanceIds = a} :: InstanceEventWindowDisassociationRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The instance tags to disassociate from the event window. Any instances
-- associated with the tags will be disassociated from the event window.
instanceEventWindowDisassociationRequest_instanceTags :: Lens.Lens' InstanceEventWindowDisassociationRequest (Prelude.Maybe [Tag])
instanceEventWindowDisassociationRequest_instanceTags = Lens.lens (\InstanceEventWindowDisassociationRequest' {instanceTags} -> instanceTags) (\s@InstanceEventWindowDisassociationRequest' {} a -> s {instanceTags = a} :: InstanceEventWindowDisassociationRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The IDs of the Dedicated Hosts to disassociate from the event window.
instanceEventWindowDisassociationRequest_dedicatedHostIds :: Lens.Lens' InstanceEventWindowDisassociationRequest (Prelude.Maybe [Prelude.Text])
instanceEventWindowDisassociationRequest_dedicatedHostIds = Lens.lens (\InstanceEventWindowDisassociationRequest' {dedicatedHostIds} -> dedicatedHostIds) (\s@InstanceEventWindowDisassociationRequest' {} a -> s {dedicatedHostIds = a} :: InstanceEventWindowDisassociationRequest) Prelude.. Lens.mapping Lens._Coerce

instance
  Prelude.Hashable
    InstanceEventWindowDisassociationRequest

instance
  Prelude.NFData
    InstanceEventWindowDisassociationRequest

instance
  Core.ToQuery
    InstanceEventWindowDisassociationRequest
  where
  toQuery InstanceEventWindowDisassociationRequest' {..} =
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
