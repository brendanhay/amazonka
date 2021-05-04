{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateVpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link, under the caller\'s account in a selected region, in
-- an asynchronous operation that typically takes 2-4 minutes to complete
-- and become operational. The caller must have permissions to create and
-- update VPC Endpoint services.
module Network.AWS.APIGateway.CreateVpcLink
  ( -- * Creating a Request
    CreateVpcLink (..),
    newCreateVpcLink,

    -- * Request Lenses
    createVpcLink_tags,
    createVpcLink_description,
    createVpcLink_name,
    createVpcLink_targetArns,

    -- * Destructuring the Response
    VpcLink (..),
    newVpcLink,

    -- * Response Lenses
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a VPC link, under the caller\'s account in a selected region, in
-- an asynchronous operation that typically takes 2-4 minutes to complete
-- and become operational. The caller must have permissions to create and
-- update VPC Endpoint services.
--
-- /See:/ 'newCreateVpcLink' smart constructor.
data CreateVpcLink = CreateVpcLink'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the VPC link.
    description :: Prelude.Maybe Prelude.Text,
    -- | [Required] The name used to label and identify the VPC link.
    name :: Prelude.Text,
    -- | [Required] The ARN of the network load balancer of the VPC targeted by
    -- the VPC link. The network load balancer must be owned by the same AWS
    -- account of the API owner.
    targetArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVpcLink_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'createVpcLink_description' - The description of the VPC link.
--
-- 'name', 'createVpcLink_name' - [Required] The name used to label and identify the VPC link.
--
-- 'targetArns', 'createVpcLink_targetArns' - [Required] The ARN of the network load balancer of the VPC targeted by
-- the VPC link. The network load balancer must be owned by the same AWS
-- account of the API owner.
newCreateVpcLink ::
  -- | 'name'
  Prelude.Text ->
  CreateVpcLink
newCreateVpcLink pName_ =
  CreateVpcLink'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      targetArns = Prelude.mempty
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createVpcLink_tags :: Lens.Lens' CreateVpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLink_tags = Lens.lens (\CreateVpcLink' {tags} -> tags) (\s@CreateVpcLink' {} a -> s {tags = a} :: CreateVpcLink) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the VPC link.
createVpcLink_description :: Lens.Lens' CreateVpcLink (Prelude.Maybe Prelude.Text)
createVpcLink_description = Lens.lens (\CreateVpcLink' {description} -> description) (\s@CreateVpcLink' {} a -> s {description = a} :: CreateVpcLink)

-- | [Required] The name used to label and identify the VPC link.
createVpcLink_name :: Lens.Lens' CreateVpcLink Prelude.Text
createVpcLink_name = Lens.lens (\CreateVpcLink' {name} -> name) (\s@CreateVpcLink' {} a -> s {name = a} :: CreateVpcLink)

-- | [Required] The ARN of the network load balancer of the VPC targeted by
-- the VPC link. The network load balancer must be owned by the same AWS
-- account of the API owner.
createVpcLink_targetArns :: Lens.Lens' CreateVpcLink [Prelude.Text]
createVpcLink_targetArns = Lens.lens (\CreateVpcLink' {targetArns} -> targetArns) (\s@CreateVpcLink' {} a -> s {targetArns = a} :: CreateVpcLink) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateVpcLink where
  type Rs CreateVpcLink = VpcLink
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateVpcLink

instance Prelude.NFData CreateVpcLink

instance Prelude.ToHeaders CreateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateVpcLink where
  toJSON CreateVpcLink' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("targetArns" Prelude..= targetArns)
          ]
      )

instance Prelude.ToPath CreateVpcLink where
  toPath = Prelude.const "/vpclinks"

instance Prelude.ToQuery CreateVpcLink where
  toQuery = Prelude.const Prelude.mempty
