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
-- Module      : Amazonka.CloudFront.CreatePublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a public key to CloudFront that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
module Amazonka.CloudFront.CreatePublicKey
  ( -- * Creating a Request
    CreatePublicKey (..),
    newCreatePublicKey,

    -- * Request Lenses
    createPublicKey_publicKeyConfig,

    -- * Destructuring the Response
    CreatePublicKeyResponse (..),
    newCreatePublicKeyResponse,

    -- * Response Lenses
    createPublicKeyResponse_eTag,
    createPublicKeyResponse_location,
    createPublicKeyResponse_publicKey,
    createPublicKeyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePublicKey' smart constructor.
data CreatePublicKey = CreatePublicKey'
  { -- | A CloudFront public key configuration.
    publicKeyConfig :: PublicKeyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKeyConfig', 'createPublicKey_publicKeyConfig' - A CloudFront public key configuration.
newCreatePublicKey ::
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  CreatePublicKey
newCreatePublicKey pPublicKeyConfig_ =
  CreatePublicKey'
    { publicKeyConfig =
        pPublicKeyConfig_
    }

-- | A CloudFront public key configuration.
createPublicKey_publicKeyConfig :: Lens.Lens' CreatePublicKey PublicKeyConfig
createPublicKey_publicKeyConfig = Lens.lens (\CreatePublicKey' {publicKeyConfig} -> publicKeyConfig) (\s@CreatePublicKey' {} a -> s {publicKeyConfig = a} :: CreatePublicKey)

instance Core.AWSRequest CreatePublicKey where
  type
    AWSResponse CreatePublicKey =
      CreatePublicKeyResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePublicKeyResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePublicKey where
  hashWithSalt _salt CreatePublicKey' {..} =
    _salt `Prelude.hashWithSalt` publicKeyConfig

instance Prelude.NFData CreatePublicKey where
  rnf CreatePublicKey' {..} =
    Prelude.rnf publicKeyConfig

instance Data.ToElement CreatePublicKey where
  toElement CreatePublicKey' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      publicKeyConfig

instance Data.ToHeaders CreatePublicKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePublicKey where
  toPath = Prelude.const "/2020-05-31/public-key"

instance Data.ToQuery CreatePublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePublicKeyResponse' smart constructor.
data CreatePublicKeyResponse = CreatePublicKeyResponse'
  { -- | The identifier for this version of the public key.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The URL of the public key.
    location :: Prelude.Maybe Prelude.Text,
    -- | The public key.
    publicKey :: Prelude.Maybe PublicKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createPublicKeyResponse_eTag' - The identifier for this version of the public key.
--
-- 'location', 'createPublicKeyResponse_location' - The URL of the public key.
--
-- 'publicKey', 'createPublicKeyResponse_publicKey' - The public key.
--
-- 'httpStatus', 'createPublicKeyResponse_httpStatus' - The response's http status code.
newCreatePublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePublicKeyResponse
newCreatePublicKeyResponse pHttpStatus_ =
  CreatePublicKeyResponse'
    { eTag = Prelude.Nothing,
      location = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the public key.
createPublicKeyResponse_eTag :: Lens.Lens' CreatePublicKeyResponse (Prelude.Maybe Prelude.Text)
createPublicKeyResponse_eTag = Lens.lens (\CreatePublicKeyResponse' {eTag} -> eTag) (\s@CreatePublicKeyResponse' {} a -> s {eTag = a} :: CreatePublicKeyResponse)

-- | The URL of the public key.
createPublicKeyResponse_location :: Lens.Lens' CreatePublicKeyResponse (Prelude.Maybe Prelude.Text)
createPublicKeyResponse_location = Lens.lens (\CreatePublicKeyResponse' {location} -> location) (\s@CreatePublicKeyResponse' {} a -> s {location = a} :: CreatePublicKeyResponse)

-- | The public key.
createPublicKeyResponse_publicKey :: Lens.Lens' CreatePublicKeyResponse (Prelude.Maybe PublicKey)
createPublicKeyResponse_publicKey = Lens.lens (\CreatePublicKeyResponse' {publicKey} -> publicKey) (\s@CreatePublicKeyResponse' {} a -> s {publicKey = a} :: CreatePublicKeyResponse)

-- | The response's http status code.
createPublicKeyResponse_httpStatus :: Lens.Lens' CreatePublicKeyResponse Prelude.Int
createPublicKeyResponse_httpStatus = Lens.lens (\CreatePublicKeyResponse' {httpStatus} -> httpStatus) (\s@CreatePublicKeyResponse' {} a -> s {httpStatus = a} :: CreatePublicKeyResponse)

instance Prelude.NFData CreatePublicKeyResponse where
  rnf CreatePublicKeyResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf httpStatus
