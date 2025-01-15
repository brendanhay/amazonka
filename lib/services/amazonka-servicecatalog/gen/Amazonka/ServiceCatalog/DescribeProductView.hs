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
-- Module      : Amazonka.ServiceCatalog.DescribeProductView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Amazonka.ServiceCatalog.DescribeProductView
  ( -- * Creating a Request
    DescribeProductView (..),
    newDescribeProductView,

    -- * Request Lenses
    describeProductView_acceptLanguage,
    describeProductView_id,

    -- * Destructuring the Response
    DescribeProductViewResponse (..),
    newDescribeProductViewResponse,

    -- * Response Lenses
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeProductView' smart constructor.
data DescribeProductView = DescribeProductView'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product view identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeProductView_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeProductView_id' - The product view identifier.
newDescribeProductView ::
  -- | 'id'
  Prelude.Text ->
  DescribeProductView
newDescribeProductView pId_ =
  DescribeProductView'
    { acceptLanguage =
        Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProductView_acceptLanguage :: Lens.Lens' DescribeProductView (Prelude.Maybe Prelude.Text)
describeProductView_acceptLanguage = Lens.lens (\DescribeProductView' {acceptLanguage} -> acceptLanguage) (\s@DescribeProductView' {} a -> s {acceptLanguage = a} :: DescribeProductView)

-- | The product view identifier.
describeProductView_id :: Lens.Lens' DescribeProductView Prelude.Text
describeProductView_id = Lens.lens (\DescribeProductView' {id} -> id) (\s@DescribeProductView' {} a -> s {id = a} :: DescribeProductView)

instance Core.AWSRequest DescribeProductView where
  type
    AWSResponse DescribeProductView =
      DescribeProductViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductViewResponse'
            Prelude.<$> (x Data..?> "ProductViewSummary")
            Prelude.<*> ( x
                            Data..?> "ProvisioningArtifacts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProductView where
  hashWithSalt _salt DescribeProductView' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeProductView where
  rnf DescribeProductView' {..} =
    Prelude.rnf acceptLanguage `Prelude.seq`
      Prelude.rnf id

instance Data.ToHeaders DescribeProductView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeProductView" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProductView where
  toJSON DescribeProductView' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DescribeProductView where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProductView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProductViewResponse' smart constructor.
data DescribeProductViewResponse = DescribeProductViewResponse'
  { -- | Summary information about the product.
    productViewSummary :: Prelude.Maybe ProductViewSummary,
    -- | Information about the provisioning artifacts for the product.
    provisioningArtifacts :: Prelude.Maybe [ProvisioningArtifact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productViewSummary', 'describeProductViewResponse_productViewSummary' - Summary information about the product.
--
-- 'provisioningArtifacts', 'describeProductViewResponse_provisioningArtifacts' - Information about the provisioning artifacts for the product.
--
-- 'httpStatus', 'describeProductViewResponse_httpStatus' - The response's http status code.
newDescribeProductViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProductViewResponse
newDescribeProductViewResponse pHttpStatus_ =
  DescribeProductViewResponse'
    { productViewSummary =
        Prelude.Nothing,
      provisioningArtifacts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the product.
describeProductViewResponse_productViewSummary :: Lens.Lens' DescribeProductViewResponse (Prelude.Maybe ProductViewSummary)
describeProductViewResponse_productViewSummary = Lens.lens (\DescribeProductViewResponse' {productViewSummary} -> productViewSummary) (\s@DescribeProductViewResponse' {} a -> s {productViewSummary = a} :: DescribeProductViewResponse)

-- | Information about the provisioning artifacts for the product.
describeProductViewResponse_provisioningArtifacts :: Lens.Lens' DescribeProductViewResponse (Prelude.Maybe [ProvisioningArtifact])
describeProductViewResponse_provisioningArtifacts = Lens.lens (\DescribeProductViewResponse' {provisioningArtifacts} -> provisioningArtifacts) (\s@DescribeProductViewResponse' {} a -> s {provisioningArtifacts = a} :: DescribeProductViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProductViewResponse_httpStatus :: Lens.Lens' DescribeProductViewResponse Prelude.Int
describeProductViewResponse_httpStatus = Lens.lens (\DescribeProductViewResponse' {httpStatus} -> httpStatus) (\s@DescribeProductViewResponse' {} a -> s {httpStatus = a} :: DescribeProductViewResponse)

instance Prelude.NFData DescribeProductViewResponse where
  rnf DescribeProductViewResponse' {..} =
    Prelude.rnf productViewSummary `Prelude.seq`
      Prelude.rnf provisioningArtifacts `Prelude.seq`
        Prelude.rnf httpStatus
