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
-- Module      : Network.AWS.EC2.Types.TransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayOptions
import Network.AWS.EC2.Types.TransitGatewayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a transit gateway.
--
-- /See:/ 'newTransitGateway' smart constructor.
data TransitGateway = TransitGateway'
  { -- | The ID of the AWS account ID that owns the transit gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The transit gateway options.
    options :: Prelude.Maybe TransitGatewayOptions,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the transit gateway.
    state :: Prelude.Maybe TransitGatewayState,
    -- | The tags for the transit gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The description of the transit gateway.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'transitGateway_ownerId' - The ID of the AWS account ID that owns the transit gateway.
--
-- 'creationTime', 'transitGateway_creationTime' - The creation time.
--
-- 'options', 'transitGateway_options' - The transit gateway options.
--
-- 'transitGatewayArn', 'transitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway.
--
-- 'state', 'transitGateway_state' - The state of the transit gateway.
--
-- 'tags', 'transitGateway_tags' - The tags for the transit gateway.
--
-- 'description', 'transitGateway_description' - The description of the transit gateway.
--
-- 'transitGatewayId', 'transitGateway_transitGatewayId' - The ID of the transit gateway.
newTransitGateway ::
  TransitGateway
newTransitGateway =
  TransitGateway'
    { ownerId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      options = Prelude.Nothing,
      transitGatewayArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The ID of the AWS account ID that owns the transit gateway.
transitGateway_ownerId :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_ownerId = Lens.lens (\TransitGateway' {ownerId} -> ownerId) (\s@TransitGateway' {} a -> s {ownerId = a} :: TransitGateway)

-- | The creation time.
transitGateway_creationTime :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.UTCTime)
transitGateway_creationTime = Lens.lens (\TransitGateway' {creationTime} -> creationTime) (\s@TransitGateway' {} a -> s {creationTime = a} :: TransitGateway) Prelude.. Lens.mapping Prelude._Time

-- | The transit gateway options.
transitGateway_options :: Lens.Lens' TransitGateway (Prelude.Maybe TransitGatewayOptions)
transitGateway_options = Lens.lens (\TransitGateway' {options} -> options) (\s@TransitGateway' {} a -> s {options = a} :: TransitGateway)

-- | The Amazon Resource Name (ARN) of the transit gateway.
transitGateway_transitGatewayArn :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_transitGatewayArn = Lens.lens (\TransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@TransitGateway' {} a -> s {transitGatewayArn = a} :: TransitGateway)

-- | The state of the transit gateway.
transitGateway_state :: Lens.Lens' TransitGateway (Prelude.Maybe TransitGatewayState)
transitGateway_state = Lens.lens (\TransitGateway' {state} -> state) (\s@TransitGateway' {} a -> s {state = a} :: TransitGateway)

-- | The tags for the transit gateway.
transitGateway_tags :: Lens.Lens' TransitGateway (Prelude.Maybe [Tag])
transitGateway_tags = Lens.lens (\TransitGateway' {tags} -> tags) (\s@TransitGateway' {} a -> s {tags = a} :: TransitGateway) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the transit gateway.
transitGateway_description :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_description = Lens.lens (\TransitGateway' {description} -> description) (\s@TransitGateway' {} a -> s {description = a} :: TransitGateway)

-- | The ID of the transit gateway.
transitGateway_transitGatewayId :: Lens.Lens' TransitGateway (Prelude.Maybe Prelude.Text)
transitGateway_transitGatewayId = Lens.lens (\TransitGateway' {transitGatewayId} -> transitGatewayId) (\s@TransitGateway' {} a -> s {transitGatewayId = a} :: TransitGateway)

instance Prelude.FromXML TransitGateway where
  parseXML x =
    TransitGateway'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "options")
      Prelude.<*> (x Prelude..@? "transitGatewayArn")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance Prelude.Hashable TransitGateway

instance Prelude.NFData TransitGateway
