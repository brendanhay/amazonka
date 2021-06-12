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
-- Module      : Network.AWS.APIGateway.GenerateClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a ClientCertificate resource.
module Network.AWS.APIGateway.GenerateClientCertificate
  ( -- * Creating a Request
    GenerateClientCertificate (..),
    newGenerateClientCertificate,

    -- * Request Lenses
    generateClientCertificate_tags,
    generateClientCertificate_description,

    -- * Destructuring the Response
    ClientCertificate (..),
    newClientCertificate,

    -- * Response Lenses
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to generate a ClientCertificate resource.
--
-- /See:/ 'newGenerateClientCertificate' smart constructor.
data GenerateClientCertificate = GenerateClientCertificate'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the ClientCertificate.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenerateClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'generateClientCertificate_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'generateClientCertificate_description' - The description of the ClientCertificate.
newGenerateClientCertificate ::
  GenerateClientCertificate
newGenerateClientCertificate =
  GenerateClientCertificate'
    { tags = Core.Nothing,
      description = Core.Nothing
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
generateClientCertificate_tags :: Lens.Lens' GenerateClientCertificate (Core.Maybe (Core.HashMap Core.Text Core.Text))
generateClientCertificate_tags = Lens.lens (\GenerateClientCertificate' {tags} -> tags) (\s@GenerateClientCertificate' {} a -> s {tags = a} :: GenerateClientCertificate) Core.. Lens.mapping Lens._Coerce

-- | The description of the ClientCertificate.
generateClientCertificate_description :: Lens.Lens' GenerateClientCertificate (Core.Maybe Core.Text)
generateClientCertificate_description = Lens.lens (\GenerateClientCertificate' {description} -> description) (\s@GenerateClientCertificate' {} a -> s {description = a} :: GenerateClientCertificate)

instance Core.AWSRequest GenerateClientCertificate where
  type
    AWSResponse GenerateClientCertificate =
      ClientCertificate
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GenerateClientCertificate

instance Core.NFData GenerateClientCertificate

instance Core.ToHeaders GenerateClientCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GenerateClientCertificate where
  toJSON GenerateClientCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath GenerateClientCertificate where
  toPath = Core.const "/clientcertificates"

instance Core.ToQuery GenerateClientCertificate where
  toQuery = Core.const Core.mempty
