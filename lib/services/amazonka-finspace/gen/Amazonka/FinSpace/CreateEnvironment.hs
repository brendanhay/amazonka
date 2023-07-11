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
-- Module      : Amazonka.FinSpace.CreateEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new FinSpace environment.
module Amazonka.FinSpace.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_dataBundles,
    createEnvironment_description,
    createEnvironment_federationMode,
    createEnvironment_federationParameters,
    createEnvironment_kmsKeyId,
    createEnvironment_superuserParameters,
    createEnvironment_tags,
    createEnvironment_name,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The list of Amazon Resource Names (ARN) of the data bundles to install.
    -- Currently supported data bundle ARNs:
    --
    -- -   @arn:aws:finspace:${Region}::data-bundle\/capital-markets-sample@ -
    --     Contains sample Capital Markets datasets, categories and controlled
    --     vocabularies.
    --
    -- -   @arn:aws:finspace:${Region}::data-bundle\/taq@ (default) - Contains
    --     trades and quotes data in addition to sample Capital Markets data.
    dataBundles :: Prelude.Maybe [Prelude.Text],
    -- | The description of the FinSpace environment to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | Authentication mode for the environment.
    --
    -- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
    --     your Identity provider.
    --
    -- -   @LOCAL@ - Users access FinSpace via email and password managed
    --     within the FinSpace environment.
    federationMode :: Prelude.Maybe FederationMode,
    -- | Configuration information when authentication mode is FEDERATED.
    federationParameters :: Prelude.Maybe FederationParameters,
    -- | The KMS key id to encrypt your data in the FinSpace environment.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for the superuser.
    superuserParameters :: Prelude.Maybe SuperuserParameters,
    -- | Add tags to your FinSpace environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the FinSpace environment to be created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataBundles', 'createEnvironment_dataBundles' - The list of Amazon Resource Names (ARN) of the data bundles to install.
-- Currently supported data bundle ARNs:
--
-- -   @arn:aws:finspace:${Region}::data-bundle\/capital-markets-sample@ -
--     Contains sample Capital Markets datasets, categories and controlled
--     vocabularies.
--
-- -   @arn:aws:finspace:${Region}::data-bundle\/taq@ (default) - Contains
--     trades and quotes data in addition to sample Capital Markets data.
--
-- 'description', 'createEnvironment_description' - The description of the FinSpace environment to be created.
--
-- 'federationMode', 'createEnvironment_federationMode' - Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
--
-- 'federationParameters', 'createEnvironment_federationParameters' - Configuration information when authentication mode is FEDERATED.
--
-- 'kmsKeyId', 'createEnvironment_kmsKeyId' - The KMS key id to encrypt your data in the FinSpace environment.
--
-- 'superuserParameters', 'createEnvironment_superuserParameters' - Configuration information for the superuser.
--
-- 'tags', 'createEnvironment_tags' - Add tags to your FinSpace environment.
--
-- 'name', 'createEnvironment_name' - The name of the FinSpace environment to be created.
newCreateEnvironment ::
  -- | 'name'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment pName_ =
  CreateEnvironment'
    { dataBundles = Prelude.Nothing,
      description = Prelude.Nothing,
      federationMode = Prelude.Nothing,
      federationParameters = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      superuserParameters = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The list of Amazon Resource Names (ARN) of the data bundles to install.
-- Currently supported data bundle ARNs:
--
-- -   @arn:aws:finspace:${Region}::data-bundle\/capital-markets-sample@ -
--     Contains sample Capital Markets datasets, categories and controlled
--     vocabularies.
--
-- -   @arn:aws:finspace:${Region}::data-bundle\/taq@ (default) - Contains
--     trades and quotes data in addition to sample Capital Markets data.
createEnvironment_dataBundles :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Prelude.Text])
createEnvironment_dataBundles = Lens.lens (\CreateEnvironment' {dataBundles} -> dataBundles) (\s@CreateEnvironment' {} a -> s {dataBundles = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The description of the FinSpace environment to be created.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | Authentication mode for the environment.
--
-- -   @FEDERATED@ - Users access FinSpace through Single Sign On (SSO) via
--     your Identity provider.
--
-- -   @LOCAL@ - Users access FinSpace via email and password managed
--     within the FinSpace environment.
createEnvironment_federationMode :: Lens.Lens' CreateEnvironment (Prelude.Maybe FederationMode)
createEnvironment_federationMode = Lens.lens (\CreateEnvironment' {federationMode} -> federationMode) (\s@CreateEnvironment' {} a -> s {federationMode = a} :: CreateEnvironment)

-- | Configuration information when authentication mode is FEDERATED.
createEnvironment_federationParameters :: Lens.Lens' CreateEnvironment (Prelude.Maybe FederationParameters)
createEnvironment_federationParameters = Lens.lens (\CreateEnvironment' {federationParameters} -> federationParameters) (\s@CreateEnvironment' {} a -> s {federationParameters = a} :: CreateEnvironment)

-- | The KMS key id to encrypt your data in the FinSpace environment.
createEnvironment_kmsKeyId :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_kmsKeyId = Lens.lens (\CreateEnvironment' {kmsKeyId} -> kmsKeyId) (\s@CreateEnvironment' {} a -> s {kmsKeyId = a} :: CreateEnvironment)

-- | Configuration information for the superuser.
createEnvironment_superuserParameters :: Lens.Lens' CreateEnvironment (Prelude.Maybe SuperuserParameters)
createEnvironment_superuserParameters = Lens.lens (\CreateEnvironment' {superuserParameters} -> superuserParameters) (\s@CreateEnvironment' {} a -> s {superuserParameters = a} :: CreateEnvironment)

-- | Add tags to your FinSpace environment.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the FinSpace environment to be created.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (x Data..?> "environmentArn")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "environmentUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` dataBundles
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` federationMode
      `Prelude.hashWithSalt` federationParameters
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` superuserParameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf dataBundles
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf federationMode
      `Prelude.seq` Prelude.rnf federationParameters
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf superuserParameters
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataBundles" Data..=) Prelude.<$> dataBundles,
            ("description" Data..=) Prelude.<$> description,
            ("federationMode" Data..=)
              Prelude.<$> federationMode,
            ("federationParameters" Data..=)
              Prelude.<$> federationParameters,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("superuserParameters" Data..=)
              Prelude.<$> superuserParameters,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateEnvironment where
  toPath = Prelude.const "/environment"

instance Data.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The Amazon Resource Name (ARN) of the FinSpace environment that you
    -- created.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for FinSpace environment that you created.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The sign-in url for the web application of the FinSpace environment you
    -- created.
    environmentUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentArn', 'createEnvironmentResponse_environmentArn' - The Amazon Resource Name (ARN) of the FinSpace environment that you
-- created.
--
-- 'environmentId', 'createEnvironmentResponse_environmentId' - The unique identifier for FinSpace environment that you created.
--
-- 'environmentUrl', 'createEnvironmentResponse_environmentUrl' - The sign-in url for the web application of the FinSpace environment you
-- created.
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse pHttpStatus_ =
  CreateEnvironmentResponse'
    { environmentArn =
        Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the FinSpace environment that you
-- created.
createEnvironmentResponse_environmentArn :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentArn = Lens.lens (\CreateEnvironmentResponse' {environmentArn} -> environmentArn) (\s@CreateEnvironmentResponse' {} a -> s {environmentArn = a} :: CreateEnvironmentResponse)

-- | The unique identifier for FinSpace environment that you created.
createEnvironmentResponse_environmentId :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentId = Lens.lens (\CreateEnvironmentResponse' {environmentId} -> environmentId) (\s@CreateEnvironmentResponse' {} a -> s {environmentId = a} :: CreateEnvironmentResponse)

-- | The sign-in url for the web application of the FinSpace environment you
-- created.
createEnvironmentResponse_environmentUrl :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentUrl = Lens.lens (\CreateEnvironmentResponse' {environmentUrl} -> environmentUrl) (\s@CreateEnvironmentResponse' {} a -> s {environmentUrl = a} :: CreateEnvironmentResponse)

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentUrl
      `Prelude.seq` Prelude.rnf httpStatus
