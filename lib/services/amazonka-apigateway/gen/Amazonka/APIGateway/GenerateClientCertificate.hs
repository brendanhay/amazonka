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
-- Module      : Amazonka.APIGateway.GenerateClientCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a ClientCertificate resource.
module Amazonka.APIGateway.GenerateClientCertificate
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
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to generate a ClientCertificate resource.
--
-- /See:/ 'newGenerateClientCertificate' smart constructor.
data GenerateClientCertificate = GenerateClientCertificate'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the ClientCertificate.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { tags = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
generateClientCertificate_tags :: Lens.Lens' GenerateClientCertificate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
generateClientCertificate_tags = Lens.lens (\GenerateClientCertificate' {tags} -> tags) (\s@GenerateClientCertificate' {} a -> s {tags = a} :: GenerateClientCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The description of the ClientCertificate.
generateClientCertificate_description :: Lens.Lens' GenerateClientCertificate (Prelude.Maybe Prelude.Text)
generateClientCertificate_description = Lens.lens (\GenerateClientCertificate' {description} -> description) (\s@GenerateClientCertificate' {} a -> s {description = a} :: GenerateClientCertificate)

instance Core.AWSRequest GenerateClientCertificate where
  type
    AWSResponse GenerateClientCertificate =
      ClientCertificate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GenerateClientCertificate where
  hashWithSalt _salt GenerateClientCertificate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description

instance Prelude.NFData GenerateClientCertificate where
  rnf GenerateClientCertificate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders GenerateClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON GenerateClientCertificate where
  toJSON GenerateClientCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath GenerateClientCertificate where
  toPath = Prelude.const "/clientcertificates"

instance Core.ToQuery GenerateClientCertificate where
  toQuery = Prelude.const Prelude.mempty
