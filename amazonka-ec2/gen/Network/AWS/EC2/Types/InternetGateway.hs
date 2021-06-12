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

-- | Describes an internet gateway.
--
-- /See:/ 'newInternetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { -- | The ID of the AWS account that owns the internet gateway.
    ownerId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the internet gateway.
    tags :: Core.Maybe [Tag],
    -- | Any VPCs attached to the internet gateway.
    attachments :: Core.Maybe [InternetGatewayAttachment],
    -- | The ID of the internet gateway.
    internetGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'internetGateway_ownerId' - The ID of the AWS account that owns the internet gateway.
--
-- 'tags', 'internetGateway_tags' - Any tags assigned to the internet gateway.
--
-- 'attachments', 'internetGateway_attachments' - Any VPCs attached to the internet gateway.
--
-- 'internetGatewayId', 'internetGateway_internetGatewayId' - The ID of the internet gateway.
newInternetGateway ::
  -- | 'internetGatewayId'
  Core.Text ->
  InternetGateway
newInternetGateway pInternetGatewayId_ =
  InternetGateway'
    { ownerId = Core.Nothing,
      tags = Core.Nothing,
      attachments = Core.Nothing,
      internetGatewayId = pInternetGatewayId_
    }

-- | The ID of the AWS account that owns the internet gateway.
internetGateway_ownerId :: Lens.Lens' InternetGateway (Core.Maybe Core.Text)
internetGateway_ownerId = Lens.lens (\InternetGateway' {ownerId} -> ownerId) (\s@InternetGateway' {} a -> s {ownerId = a} :: InternetGateway)

-- | Any tags assigned to the internet gateway.
internetGateway_tags :: Lens.Lens' InternetGateway (Core.Maybe [Tag])
internetGateway_tags = Lens.lens (\InternetGateway' {tags} -> tags) (\s@InternetGateway' {} a -> s {tags = a} :: InternetGateway) Core.. Lens.mapping Lens._Coerce

-- | Any VPCs attached to the internet gateway.
internetGateway_attachments :: Lens.Lens' InternetGateway (Core.Maybe [InternetGatewayAttachment])
internetGateway_attachments = Lens.lens (\InternetGateway' {attachments} -> attachments) (\s@InternetGateway' {} a -> s {attachments = a} :: InternetGateway) Core.. Lens.mapping Lens._Coerce

-- | The ID of the internet gateway.
internetGateway_internetGatewayId :: Lens.Lens' InternetGateway Core.Text
internetGateway_internetGatewayId = Lens.lens (\InternetGateway' {internetGatewayId} -> internetGatewayId) (\s@InternetGateway' {} a -> s {internetGatewayId = a} :: InternetGateway)

instance Core.FromXML InternetGateway where
  parseXML x =
    InternetGateway'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "attachmentSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "internetGatewayId")

instance Core.Hashable InternetGateway

instance Core.NFData InternetGateway
