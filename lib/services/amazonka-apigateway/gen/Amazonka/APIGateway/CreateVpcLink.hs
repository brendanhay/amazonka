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
-- Module      : Amazonka.APIGateway.CreateVpcLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link, under the caller\'s account in a selected region, in
-- an asynchronous operation that typically takes 2-4 minutes to complete
-- and become operational. The caller must have permissions to create and
-- update VPC Endpoint services.
module Amazonka.APIGateway.CreateVpcLink
  ( -- * Creating a Request
    CreateVpcLink (..),
    newCreateVpcLink,

    -- * Request Lenses
    createVpcLink_description,
    createVpcLink_tags,
    createVpcLink_name,
    createVpcLink_targetArns,

    -- * Destructuring the Response
    VpcLink (..),
    newVpcLink,

    -- * Response Lenses
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a VPC link, under the caller\'s account in a selected region, in
-- an asynchronous operation that typically takes 2-4 minutes to complete
-- and become operational. The caller must have permissions to create and
-- update VPC Endpoint services.
--
-- /See:/ 'newCreateVpcLink' smart constructor.
data CreateVpcLink = CreateVpcLink'
  { -- | The description of the VPC link.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name used to label and identify the VPC link.
    name :: Prelude.Text,
    -- | The ARN of the network load balancer of the VPC targeted by the VPC
    -- link. The network load balancer must be owned by the same AWS account of
    -- the API owner.
    targetArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createVpcLink_description' - The description of the VPC link.
--
-- 'tags', 'createVpcLink_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'name', 'createVpcLink_name' - The name used to label and identify the VPC link.
--
-- 'targetArns', 'createVpcLink_targetArns' - The ARN of the network load balancer of the VPC targeted by the VPC
-- link. The network load balancer must be owned by the same AWS account of
-- the API owner.
newCreateVpcLink ::
  -- | 'name'
  Prelude.Text ->
  CreateVpcLink
newCreateVpcLink pName_ =
  CreateVpcLink'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      targetArns = Prelude.mempty
    }

-- | The description of the VPC link.
createVpcLink_description :: Lens.Lens' CreateVpcLink (Prelude.Maybe Prelude.Text)
createVpcLink_description = Lens.lens (\CreateVpcLink' {description} -> description) (\s@CreateVpcLink' {} a -> s {description = a} :: CreateVpcLink)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createVpcLink_tags :: Lens.Lens' CreateVpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLink_tags = Lens.lens (\CreateVpcLink' {tags} -> tags) (\s@CreateVpcLink' {} a -> s {tags = a} :: CreateVpcLink) Prelude.. Lens.mapping Lens.coerced

-- | The name used to label and identify the VPC link.
createVpcLink_name :: Lens.Lens' CreateVpcLink Prelude.Text
createVpcLink_name = Lens.lens (\CreateVpcLink' {name} -> name) (\s@CreateVpcLink' {} a -> s {name = a} :: CreateVpcLink)

-- | The ARN of the network load balancer of the VPC targeted by the VPC
-- link. The network load balancer must be owned by the same AWS account of
-- the API owner.
createVpcLink_targetArns :: Lens.Lens' CreateVpcLink [Prelude.Text]
createVpcLink_targetArns = Lens.lens (\CreateVpcLink' {targetArns} -> targetArns) (\s@CreateVpcLink' {} a -> s {targetArns = a} :: CreateVpcLink) Prelude.. Lens.coerced

instance Core.AWSRequest CreateVpcLink where
  type AWSResponse CreateVpcLink = VpcLink
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateVpcLink where
  hashWithSalt _salt CreateVpcLink' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetArns

instance Prelude.NFData CreateVpcLink where
  rnf CreateVpcLink' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf targetArns

instance Data.ToHeaders CreateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateVpcLink where
  toJSON CreateVpcLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("targetArns" Data..= targetArns)
          ]
      )

instance Data.ToPath CreateVpcLink where
  toPath = Prelude.const "/vpclinks"

instance Data.ToQuery CreateVpcLink where
  toQuery = Prelude.const Prelude.mempty
