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
-- Module      : Network.AWS.EC2.Types.InternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an internet gateway.
--
-- /See:/ 'newInternetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { -- | Any VPCs attached to the internet gateway.
    attachments :: Prelude.Maybe [InternetGatewayAttachment],
    -- | The ID of the Amazon Web Services account that owns the internet
    -- gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the internet gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the internet gateway.
    internetGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'internetGateway_attachments' - Any VPCs attached to the internet gateway.
--
-- 'ownerId', 'internetGateway_ownerId' - The ID of the Amazon Web Services account that owns the internet
-- gateway.
--
-- 'tags', 'internetGateway_tags' - Any tags assigned to the internet gateway.
--
-- 'internetGatewayId', 'internetGateway_internetGatewayId' - The ID of the internet gateway.
newInternetGateway ::
  -- | 'internetGatewayId'
  Prelude.Text ->
  InternetGateway
newInternetGateway pInternetGatewayId_ =
  InternetGateway'
    { attachments = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      tags = Prelude.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | Any VPCs attached to the internet gateway.
internetGateway_attachments :: Lens.Lens' InternetGateway (Prelude.Maybe [InternetGatewayAttachment])
internetGateway_attachments = Lens.lens (\InternetGateway' {attachments} -> attachments) (\s@InternetGateway' {} a -> s {attachments = a} :: InternetGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the internet
-- gateway.
internetGateway_ownerId :: Lens.Lens' InternetGateway (Prelude.Maybe Prelude.Text)
internetGateway_ownerId = Lens.lens (\InternetGateway' {ownerId} -> ownerId) (\s@InternetGateway' {} a -> s {ownerId = a} :: InternetGateway)

-- | Any tags assigned to the internet gateway.
internetGateway_tags :: Lens.Lens' InternetGateway (Prelude.Maybe [Tag])
internetGateway_tags = Lens.lens (\InternetGateway' {tags} -> tags) (\s@InternetGateway' {} a -> s {tags = a} :: InternetGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the internet gateway.
internetGateway_internetGatewayId :: Lens.Lens' InternetGateway Prelude.Text
internetGateway_internetGatewayId = Lens.lens (\InternetGateway' {internetGatewayId} -> internetGatewayId) (\s@InternetGateway' {} a -> s {internetGatewayId = a} :: InternetGateway)

instance Core.FromXML InternetGateway where
  parseXML x =
    InternetGateway'
      Prelude.<$> ( x Core..@? "attachmentSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@ "internetGatewayId")

instance Prelude.Hashable InternetGateway

instance Prelude.NFData InternetGateway
