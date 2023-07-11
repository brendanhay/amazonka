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
-- Module      : Amazonka.EC2.Types.LocalGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a local gateway.
--
-- /See:/ 'newLocalGateway' smart constructor.
data LocalGateway = LocalGateway'
  { -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the local gateway.
    state :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the local gateway.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayId', 'localGateway_localGatewayId' - The ID of the local gateway.
--
-- 'outpostArn', 'localGateway_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'ownerId', 'localGateway_ownerId' - The ID of the Amazon Web Services account that owns the local gateway.
--
-- 'state', 'localGateway_state' - The state of the local gateway.
--
-- 'tags', 'localGateway_tags' - The tags assigned to the local gateway.
newLocalGateway ::
  LocalGateway
newLocalGateway =
  LocalGateway'
    { localGatewayId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the local gateway.
localGateway_localGatewayId :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_localGatewayId = Lens.lens (\LocalGateway' {localGatewayId} -> localGatewayId) (\s@LocalGateway' {} a -> s {localGatewayId = a} :: LocalGateway)

-- | The Amazon Resource Name (ARN) of the Outpost.
localGateway_outpostArn :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_outpostArn = Lens.lens (\LocalGateway' {outpostArn} -> outpostArn) (\s@LocalGateway' {} a -> s {outpostArn = a} :: LocalGateway)

-- | The ID of the Amazon Web Services account that owns the local gateway.
localGateway_ownerId :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_ownerId = Lens.lens (\LocalGateway' {ownerId} -> ownerId) (\s@LocalGateway' {} a -> s {ownerId = a} :: LocalGateway)

-- | The state of the local gateway.
localGateway_state :: Lens.Lens' LocalGateway (Prelude.Maybe Prelude.Text)
localGateway_state = Lens.lens (\LocalGateway' {state} -> state) (\s@LocalGateway' {} a -> s {state = a} :: LocalGateway)

-- | The tags assigned to the local gateway.
localGateway_tags :: Lens.Lens' LocalGateway (Prelude.Maybe [Tag])
localGateway_tags = Lens.lens (\LocalGateway' {tags} -> tags) (\s@LocalGateway' {} a -> s {tags = a} :: LocalGateway) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML LocalGateway where
  parseXML x =
    LocalGateway'
      Prelude.<$> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable LocalGateway where
  hashWithSalt _salt LocalGateway' {..} =
    _salt
      `Prelude.hashWithSalt` localGatewayId
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData LocalGateway where
  rnf LocalGateway' {..} =
    Prelude.rnf localGatewayId
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
