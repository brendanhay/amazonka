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
-- Module      : Amazonka.SecurityHub.EnableSecurityHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Security Hub for your account in the current Region or the
-- Region you specify in the request.
--
-- When you enable Security Hub, you grant to Security Hub the permissions
-- necessary to gather findings from other services that are integrated
-- with Security Hub.
--
-- When you use the @EnableSecurityHub@ operation to enable Security Hub,
-- you also automatically enable the following standards:
--
-- -   Center for Internet Security (CIS) Amazon Web Services Foundations
--     Benchmark v1.2.0
--
-- -   Amazon Web Services Foundational Security Best Practices
--
-- Other standards are not automatically enabled.
--
-- To opt out of automatically enabled standards, set
-- @EnableDefaultStandards@ to @false@.
--
-- After you enable Security Hub, to enable a standard, use the
-- @BatchEnableStandards@ operation. To disable a standard, use the
-- @BatchDisableStandards@ operation.
--
-- To learn more, see the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-settingup.html setup information>
-- in the /Security Hub User Guide/.
module Amazonka.SecurityHub.EnableSecurityHub
  ( -- * Creating a Request
    EnableSecurityHub (..),
    newEnableSecurityHub,

    -- * Request Lenses
    enableSecurityHub_controlFindingGenerator,
    enableSecurityHub_enableDefaultStandards,
    enableSecurityHub_tags,

    -- * Destructuring the Response
    EnableSecurityHubResponse (..),
    newEnableSecurityHubResponse,

    -- * Response Lenses
    enableSecurityHubResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newEnableSecurityHub' smart constructor.
data EnableSecurityHub = EnableSecurityHub'
  { -- | This field, used when enabling Security Hub, specifies whether the
    -- calling account has consolidated control findings turned on. If the
    -- value for this field is set to @SECURITY_CONTROL@, Security Hub
    -- generates a single finding for a control check even when the check
    -- applies to multiple enabled standards.
    --
    -- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
    -- generates separate findings for a control check when the check applies
    -- to multiple enabled standards.
    --
    -- The value for this field in a member account matches the value in the
    -- administrator account. For accounts that aren\'t part of an
    -- organization, the default value of this field is @SECURITY_CONTROL@ if
    -- you enabled Security Hub on or after February 23, 2023.
    controlFindingGenerator :: Prelude.Maybe ControlFindingGenerator,
    -- | Whether to enable the security standards that Security Hub has
    -- designated as automatically enabled. If you do not provide a value for
    -- @EnableDefaultStandards@, it is set to @true@. To not enable the
    -- automatically enabled standards, set @EnableDefaultStandards@ to
    -- @false@.
    enableDefaultStandards :: Prelude.Maybe Prelude.Bool,
    -- | The tags to add to the hub resource when you enable Security Hub.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSecurityHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlFindingGenerator', 'enableSecurityHub_controlFindingGenerator' - This field, used when enabling Security Hub, specifies whether the
-- calling account has consolidated control findings turned on. If the
-- value for this field is set to @SECURITY_CONTROL@, Security Hub
-- generates a single finding for a control check even when the check
-- applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- The value for this field in a member account matches the value in the
-- administrator account. For accounts that aren\'t part of an
-- organization, the default value of this field is @SECURITY_CONTROL@ if
-- you enabled Security Hub on or after February 23, 2023.
--
-- 'enableDefaultStandards', 'enableSecurityHub_enableDefaultStandards' - Whether to enable the security standards that Security Hub has
-- designated as automatically enabled. If you do not provide a value for
-- @EnableDefaultStandards@, it is set to @true@. To not enable the
-- automatically enabled standards, set @EnableDefaultStandards@ to
-- @false@.
--
-- 'tags', 'enableSecurityHub_tags' - The tags to add to the hub resource when you enable Security Hub.
newEnableSecurityHub ::
  EnableSecurityHub
newEnableSecurityHub =
  EnableSecurityHub'
    { controlFindingGenerator =
        Prelude.Nothing,
      enableDefaultStandards = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | This field, used when enabling Security Hub, specifies whether the
-- calling account has consolidated control findings turned on. If the
-- value for this field is set to @SECURITY_CONTROL@, Security Hub
-- generates a single finding for a control check even when the check
-- applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- The value for this field in a member account matches the value in the
-- administrator account. For accounts that aren\'t part of an
-- organization, the default value of this field is @SECURITY_CONTROL@ if
-- you enabled Security Hub on or after February 23, 2023.
enableSecurityHub_controlFindingGenerator :: Lens.Lens' EnableSecurityHub (Prelude.Maybe ControlFindingGenerator)
enableSecurityHub_controlFindingGenerator = Lens.lens (\EnableSecurityHub' {controlFindingGenerator} -> controlFindingGenerator) (\s@EnableSecurityHub' {} a -> s {controlFindingGenerator = a} :: EnableSecurityHub)

-- | Whether to enable the security standards that Security Hub has
-- designated as automatically enabled. If you do not provide a value for
-- @EnableDefaultStandards@, it is set to @true@. To not enable the
-- automatically enabled standards, set @EnableDefaultStandards@ to
-- @false@.
enableSecurityHub_enableDefaultStandards :: Lens.Lens' EnableSecurityHub (Prelude.Maybe Prelude.Bool)
enableSecurityHub_enableDefaultStandards = Lens.lens (\EnableSecurityHub' {enableDefaultStandards} -> enableDefaultStandards) (\s@EnableSecurityHub' {} a -> s {enableDefaultStandards = a} :: EnableSecurityHub)

-- | The tags to add to the hub resource when you enable Security Hub.
enableSecurityHub_tags :: Lens.Lens' EnableSecurityHub (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
enableSecurityHub_tags = Lens.lens (\EnableSecurityHub' {tags} -> tags) (\s@EnableSecurityHub' {} a -> s {tags = a} :: EnableSecurityHub) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest EnableSecurityHub where
  type
    AWSResponse EnableSecurityHub =
      EnableSecurityHubResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableSecurityHubResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableSecurityHub where
  hashWithSalt _salt EnableSecurityHub' {..} =
    _salt
      `Prelude.hashWithSalt` controlFindingGenerator
      `Prelude.hashWithSalt` enableDefaultStandards
      `Prelude.hashWithSalt` tags

instance Prelude.NFData EnableSecurityHub where
  rnf EnableSecurityHub' {..} =
    Prelude.rnf controlFindingGenerator
      `Prelude.seq` Prelude.rnf enableDefaultStandards
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders EnableSecurityHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableSecurityHub where
  toJSON EnableSecurityHub' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ControlFindingGenerator" Data..=)
              Prelude.<$> controlFindingGenerator,
            ("EnableDefaultStandards" Data..=)
              Prelude.<$> enableDefaultStandards,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath EnableSecurityHub where
  toPath = Prelude.const "/accounts"

instance Data.ToQuery EnableSecurityHub where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableSecurityHubResponse' smart constructor.
data EnableSecurityHubResponse = EnableSecurityHubResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSecurityHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableSecurityHubResponse_httpStatus' - The response's http status code.
newEnableSecurityHubResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSecurityHubResponse
newEnableSecurityHubResponse pHttpStatus_ =
  EnableSecurityHubResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
enableSecurityHubResponse_httpStatus :: Lens.Lens' EnableSecurityHubResponse Prelude.Int
enableSecurityHubResponse_httpStatus = Lens.lens (\EnableSecurityHubResponse' {httpStatus} -> httpStatus) (\s@EnableSecurityHubResponse' {} a -> s {httpStatus = a} :: EnableSecurityHubResponse)

instance Prelude.NFData EnableSecurityHubResponse where
  rnf EnableSecurityHubResponse' {..} =
    Prelude.rnf httpStatus
