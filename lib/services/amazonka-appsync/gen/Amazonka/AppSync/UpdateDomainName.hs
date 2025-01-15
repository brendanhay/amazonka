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
-- Module      : Amazonka.AppSync.UpdateDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a custom @DomainName@ object.
module Amazonka.AppSync.UpdateDomainName
  ( -- * Creating a Request
    UpdateDomainName (..),
    newUpdateDomainName,

    -- * Request Lenses
    updateDomainName_description,
    updateDomainName_domainName,

    -- * Destructuring the Response
    UpdateDomainNameResponse (..),
    newUpdateDomainNameResponse,

    -- * Response Lenses
    updateDomainNameResponse_domainNameConfig,
    updateDomainNameResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { -- | A description of the @DomainName@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDomainName_description' - A description of the @DomainName@.
--
-- 'domainName', 'updateDomainName_domainName' - The domain name.
newUpdateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainName
newUpdateDomainName pDomainName_ =
  UpdateDomainName'
    { description = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | A description of the @DomainName@.
updateDomainName_description :: Lens.Lens' UpdateDomainName (Prelude.Maybe Prelude.Text)
updateDomainName_description = Lens.lens (\UpdateDomainName' {description} -> description) (\s@UpdateDomainName' {} a -> s {description = a} :: UpdateDomainName)

-- | The domain name.
updateDomainName_domainName :: Lens.Lens' UpdateDomainName Prelude.Text
updateDomainName_domainName = Lens.lens (\UpdateDomainName' {domainName} -> domainName) (\s@UpdateDomainName' {} a -> s {domainName = a} :: UpdateDomainName)

instance Core.AWSRequest UpdateDomainName where
  type
    AWSResponse UpdateDomainName =
      UpdateDomainNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainNameResponse'
            Prelude.<$> (x Data..?> "domainNameConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainName where
  hashWithSalt _salt UpdateDomainName' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainName where
  rnf UpdateDomainName' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomainName where
  toJSON UpdateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [("description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateDomainName where
  toPath UpdateDomainName' {..} =
    Prelude.mconcat
      ["/v1/domainnames/", Data.toBS domainName]

instance Data.ToQuery UpdateDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainNameResponse' smart constructor.
data UpdateDomainNameResponse = UpdateDomainNameResponse'
  { -- | The configuration for the @DomainName@.
    domainNameConfig :: Prelude.Maybe DomainNameConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfig', 'updateDomainNameResponse_domainNameConfig' - The configuration for the @DomainName@.
--
-- 'httpStatus', 'updateDomainNameResponse_httpStatus' - The response's http status code.
newUpdateDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainNameResponse
newUpdateDomainNameResponse pHttpStatus_ =
  UpdateDomainNameResponse'
    { domainNameConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration for the @DomainName@.
updateDomainNameResponse_domainNameConfig :: Lens.Lens' UpdateDomainNameResponse (Prelude.Maybe DomainNameConfig)
updateDomainNameResponse_domainNameConfig = Lens.lens (\UpdateDomainNameResponse' {domainNameConfig} -> domainNameConfig) (\s@UpdateDomainNameResponse' {} a -> s {domainNameConfig = a} :: UpdateDomainNameResponse)

-- | The response's http status code.
updateDomainNameResponse_httpStatus :: Lens.Lens' UpdateDomainNameResponse Prelude.Int
updateDomainNameResponse_httpStatus = Lens.lens (\UpdateDomainNameResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainNameResponse' {} a -> s {httpStatus = a} :: UpdateDomainNameResponse)

instance Prelude.NFData UpdateDomainNameResponse where
  rnf UpdateDomainNameResponse' {..} =
    Prelude.rnf domainNameConfig `Prelude.seq`
      Prelude.rnf httpStatus
