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
-- Module      : Amazonka.FraudDetector.UpdateRuleMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a rule\'s metadata. The description attribute can be updated.
module Amazonka.FraudDetector.UpdateRuleMetadata
  ( -- * Creating a Request
    UpdateRuleMetadata (..),
    newUpdateRuleMetadata,

    -- * Request Lenses
    updateRuleMetadata_rule,
    updateRuleMetadata_description,

    -- * Destructuring the Response
    UpdateRuleMetadataResponse (..),
    newUpdateRuleMetadataResponse,

    -- * Response Lenses
    updateRuleMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRuleMetadata' smart constructor.
data UpdateRuleMetadata = UpdateRuleMetadata'
  { -- | The rule to update.
    rule :: Rule,
    -- | The rule description.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'updateRuleMetadata_rule' - The rule to update.
--
-- 'description', 'updateRuleMetadata_description' - The rule description.
newUpdateRuleMetadata ::
  -- | 'rule'
  Rule ->
  -- | 'description'
  Prelude.Text ->
  UpdateRuleMetadata
newUpdateRuleMetadata pRule_ pDescription_ =
  UpdateRuleMetadata'
    { rule = pRule_,
      description = pDescription_
    }

-- | The rule to update.
updateRuleMetadata_rule :: Lens.Lens' UpdateRuleMetadata Rule
updateRuleMetadata_rule = Lens.lens (\UpdateRuleMetadata' {rule} -> rule) (\s@UpdateRuleMetadata' {} a -> s {rule = a} :: UpdateRuleMetadata)

-- | The rule description.
updateRuleMetadata_description :: Lens.Lens' UpdateRuleMetadata Prelude.Text
updateRuleMetadata_description = Lens.lens (\UpdateRuleMetadata' {description} -> description) (\s@UpdateRuleMetadata' {} a -> s {description = a} :: UpdateRuleMetadata)

instance Core.AWSRequest UpdateRuleMetadata where
  type
    AWSResponse UpdateRuleMetadata =
      UpdateRuleMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRuleMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuleMetadata where
  hashWithSalt _salt UpdateRuleMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` rule
      `Prelude.hashWithSalt` description

instance Prelude.NFData UpdateRuleMetadata where
  rnf UpdateRuleMetadata' {..} =
    Prelude.rnf rule `Prelude.seq`
      Prelude.rnf description

instance Data.ToHeaders UpdateRuleMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateRuleMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuleMetadata where
  toJSON UpdateRuleMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("rule" Data..= rule),
            Prelude.Just ("description" Data..= description)
          ]
      )

instance Data.ToPath UpdateRuleMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRuleMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleMetadataResponse' smart constructor.
data UpdateRuleMetadataResponse = UpdateRuleMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRuleMetadataResponse_httpStatus' - The response's http status code.
newUpdateRuleMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleMetadataResponse
newUpdateRuleMetadataResponse pHttpStatus_ =
  UpdateRuleMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRuleMetadataResponse_httpStatus :: Lens.Lens' UpdateRuleMetadataResponse Prelude.Int
updateRuleMetadataResponse_httpStatus = Lens.lens (\UpdateRuleMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleMetadataResponse' {} a -> s {httpStatus = a} :: UpdateRuleMetadataResponse)

instance Prelude.NFData UpdateRuleMetadataResponse where
  rnf UpdateRuleMetadataResponse' {..} =
    Prelude.rnf httpStatus
