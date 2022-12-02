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
-- Module      : Amazonka.EC2.Types.TransitGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayOptions
import Amazonka.EC2.Types.TransitGatewayState
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway.
--
-- /See:/ 'newTransitGateway' smart constructor.
data TransitGateway = TransitGateway'
  { -- | The tags for the transit gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Amazon Web Services account that owns the transit gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the transit gateway.
    state :: Prelude.Maybe TransitGatewayState,
    -- | The description of the transit gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway options.
    options :: Prelude.Maybe TransitGatewayOptions,
    -- | The creation time.
    creationTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'transitGateway_tags' - The tags for the transit gateway.
--
-- 'ownerId', 'transitGateway_ownerId' - The ID of the Amazon Web Services account that owns the transit gateway.
--
-- 'transitGatewayId', 'transitGateway_transitGatewayId' - The ID of the transit gateway.
--
-- 'transitGatewayArn', 'transitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway.
--
-- 'state', 'transitGateway_state' - The state of the transit gateway.
--
-- 'description', 'transitGateway_description' - The description of the transit gateway.
--
-- 'options', 'transitGateway_options' - The transit gateway options.
--
-- 'creationTime', 'transitGateway_creationTime' - The creation time.
newTransitGateway ::
  TransitGateway
newTransitGateway =
  TransitGateway'
    { tags = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      transitGatewayArn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      options = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The tags for the transit gateway.
transitGateway_tags :: Lens.Lens' TransitGateway (Prelude.Maybe [Tag])
transitGateway_tags = Lens.lens (\TransitGateway' {tags} -> tags) (\s@TransitGateway' {} a -> s {tags = a} :: TransitGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the transit gateway.
transitGateway_ownerId :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_ownerId = Lens.lens (\TransitGateway' {ownerId} -> ownerId) (\s@TransitGateway' {} a -> s {ownerId = a} :: TransitGateway)

-- | The ID of the transit gateway.
transitGateway_transitGatewayId :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_transitGatewayId = Lens.lens (\TransitGateway' {transitGatewayId} -> transitGatewayId) (\s@TransitGateway' {} a -> s {transitGatewayId = a} :: TransitGateway)

-- | The Amazon Resource Name (ARN) of the transit gateway.
transitGateway_transitGatewayArn :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_transitGatewayArn = Lens.lens (\TransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@TransitGateway' {} a -> s {transitGatewayArn = a} :: TransitGateway)

-- | The state of the transit gateway.
transitGateway_state :: Lens.Lens' TransitGateway (Prelude.Maybe TransitGatewayState)
transitGateway_state = Lens.lens (\TransitGateway' {state} -> state) (\s@TransitGateway' {} a -> s {state = a} :: TransitGateway)

-- | The description of the transit gateway.
transitGateway_description :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_description = Lens.lens (\TransitGateway' {description} -> description) (\s@TransitGateway' {} a -> s {description = a} :: TransitGateway)

-- | The transit gateway options.
transitGateway_options :: Lens.Lens' TransitGateway (Prelude.Maybe TransitGatewayOptions)
transitGateway_options = Lens.lens (\TransitGateway' {options} -> options) (\s@TransitGateway' {} a -> s {options = a} :: TransitGateway)

-- | The creation time.
transitGateway_creationTime :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.UTCTime)
transitGateway_creationTime = Lens.lens (\TransitGateway' {creationTime} -> creationTime) (\s@TransitGateway' {} a -> s {creationTime = a} :: TransitGateway) Prelude.. Lens.mapping Data._Time

instance Data.FromXML TransitGateway where
  parseXML x =
    TransitGateway'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "transitGatewayArn")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "options")
      Prelude.<*> (x Data..@? "creationTime")

instance Prelude.Hashable TransitGateway where
  hashWithSalt _salt TransitGateway' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` transitGatewayArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData TransitGateway where
  rnf TransitGateway' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf transitGatewayArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf creationTime
