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
-- Module      : Network.AWS.EC2.Types.LocalGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a local gateway.
--
-- /See:/ 'newLocalGateway' smart constructor.
data LocalGateway = LocalGateway'
  { -- | The AWS account ID that owns the local gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The state of the local gateway.
    state :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the local gateway.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LocalGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGateway_ownerId' - The AWS account ID that owns the local gateway.
--
-- 'outpostArn', 'localGateway_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'localGatewayId', 'localGateway_localGatewayId' - The ID of the local gateway.
--
-- 'state', 'localGateway_state' - The state of the local gateway.
--
-- 'tags', 'localGateway_tags' - The tags assigned to the local gateway.
newLocalGateway ::
  LocalGateway
newLocalGateway =
  LocalGateway'
    { ownerId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The AWS account ID that owns the local gateway.
localGateway_ownerId :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_ownerId = Lens.lens (\LocalGateway' {ownerId} -> ownerId) (\s@LocalGateway' {} a -> s {ownerId = a} :: LocalGateway)

-- | The Amazon Resource Name (ARN) of the Outpost.
localGateway_outpostArn :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_outpostArn = Lens.lens (\LocalGateway' {outpostArn} -> outpostArn) (\s@LocalGateway' {} a -> s {outpostArn = a} :: LocalGateway)

-- | The ID of the local gateway.
localGateway_localGatewayId :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_localGatewayId = Lens.lens (\LocalGateway' {localGatewayId} -> localGatewayId) (\s@LocalGateway' {} a -> s {localGatewayId = a} :: LocalGateway)

-- | The state of the local gateway.
localGateway_state :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_state = Lens.lens (\LocalGateway' {state} -> state) (\s@LocalGateway' {} a -> s {state = a} :: LocalGateway)

-- | The tags assigned to the local gateway.
localGateway_tags :: Lens.Lens' LocalGateway (Prelude.Maybe [Tag])
localGateway_tags = Lens.lens (\LocalGateway' {tags} -> tags) (\s@LocalGateway' {} a -> s {tags = a} :: LocalGateway) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML LocalGateway where
  parseXML x =
    LocalGateway'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "localGatewayId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable LocalGateway

instance Prelude.NFData LocalGateway
