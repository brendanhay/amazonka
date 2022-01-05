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
-- Module      : Amazonka.APIGateway.Types.VpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.VpcLink where

import Amazonka.APIGateway.Types.VpcLinkStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An API Gateway VPC link for a RestApi to access resources in an Amazon
-- Virtual Private Cloud (VPC).
--
-- To enable access to a resource in an Amazon Virtual Private Cloud
-- through Amazon API Gateway, you, as an API developer, create a VpcLink
-- resource targeted for one or more network load balancers of the VPC and
-- then integrate an API method with a private integration that uses the
-- VpcLink. The private integration has an integration type of @HTTP@ or
-- @HTTP_PROXY@ and has a connection type of @VPC_LINK@. The integration
-- uses the @connectionId@ property to identify the VpcLink used.
--
-- /See:/ 'newVpcLink' smart constructor.
data VpcLink = VpcLink'
  { -- | The status of the VPC link. The valid values are @AVAILABLE@, @PENDING@,
    -- @DELETING@, or @FAILED@. Deploying an API will wait if the status is
    -- @PENDING@ and will fail if the status is @DELETING@.
    status :: Prelude.Maybe VpcLinkStatus,
    -- | The ARN of the network load balancer of the VPC targeted by the VPC
    -- link. The network load balancer must be owned by the same AWS account of
    -- the API owner.
    targetArns :: Prelude.Maybe [Prelude.Text],
    -- | The name used to label and identify the VPC link.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description about the VPC link status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VpcLink. It is used in an Integration to reference
    -- this VpcLink.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the VPC link.
    description :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'vpcLink_status' - The status of the VPC link. The valid values are @AVAILABLE@, @PENDING@,
-- @DELETING@, or @FAILED@. Deploying an API will wait if the status is
-- @PENDING@ and will fail if the status is @DELETING@.
--
-- 'targetArns', 'vpcLink_targetArns' - The ARN of the network load balancer of the VPC targeted by the VPC
-- link. The network load balancer must be owned by the same AWS account of
-- the API owner.
--
-- 'name', 'vpcLink_name' - The name used to label and identify the VPC link.
--
-- 'statusMessage', 'vpcLink_statusMessage' - A description about the VPC link status.
--
-- 'id', 'vpcLink_id' - The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
--
-- 'description', 'vpcLink_description' - The description of the VPC link.
--
-- 'tags', 'vpcLink_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
newVpcLink ::
  VpcLink
newVpcLink =
  VpcLink'
    { status = Prelude.Nothing,
      targetArns = Prelude.Nothing,
      name = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the VPC link. The valid values are @AVAILABLE@, @PENDING@,
-- @DELETING@, or @FAILED@. Deploying an API will wait if the status is
-- @PENDING@ and will fail if the status is @DELETING@.
vpcLink_status :: Lens.Lens' VpcLink (Prelude.Maybe VpcLinkStatus)
vpcLink_status = Lens.lens (\VpcLink' {status} -> status) (\s@VpcLink' {} a -> s {status = a} :: VpcLink)

-- | The ARN of the network load balancer of the VPC targeted by the VPC
-- link. The network load balancer must be owned by the same AWS account of
-- the API owner.
vpcLink_targetArns :: Lens.Lens' VpcLink (Prelude.Maybe [Prelude.Text])
vpcLink_targetArns = Lens.lens (\VpcLink' {targetArns} -> targetArns) (\s@VpcLink' {} a -> s {targetArns = a} :: VpcLink) Prelude.. Lens.mapping Lens.coerced

-- | The name used to label and identify the VPC link.
vpcLink_name :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.Text)
vpcLink_name = Lens.lens (\VpcLink' {name} -> name) (\s@VpcLink' {} a -> s {name = a} :: VpcLink)

-- | A description about the VPC link status.
vpcLink_statusMessage :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.Text)
vpcLink_statusMessage = Lens.lens (\VpcLink' {statusMessage} -> statusMessage) (\s@VpcLink' {} a -> s {statusMessage = a} :: VpcLink)

-- | The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
vpcLink_id :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.Text)
vpcLink_id = Lens.lens (\VpcLink' {id} -> id) (\s@VpcLink' {} a -> s {id = a} :: VpcLink)

-- | The description of the VPC link.
vpcLink_description :: Lens.Lens' VpcLink (Prelude.Maybe Prelude.Text)
vpcLink_description = Lens.lens (\VpcLink' {description} -> description) (\s@VpcLink' {} a -> s {description = a} :: VpcLink)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
vpcLink_tags :: Lens.Lens' VpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vpcLink_tags = Lens.lens (\VpcLink' {tags} -> tags) (\s@VpcLink' {} a -> s {tags = a} :: VpcLink) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VpcLink where
  parseJSON =
    Core.withObject
      "VpcLink"
      ( \x ->
          VpcLink'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "targetArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable VpcLink where
  hashWithSalt _salt VpcLink' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetArns
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags

instance Prelude.NFData VpcLink where
  rnf VpcLink' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetArns
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
