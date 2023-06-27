{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types.ValidationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.ValidationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.ValidationMode

-- | A structure that contains Cedar policy validation settings for the
-- policy store. The validation mode determines which validation failures
-- that Cedar considers serious enough to block acceptance of a new or
-- edited static policy or policy template.
--
-- This data type is used as a request parameter in the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicyStore.html CreatePolicyStore>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore.html UpdatePolicyStore>
-- operations.
--
-- /See:/ 'newValidationSettings' smart constructor.
data ValidationSettings = ValidationSettings'
  { -- | The validation mode currently configured for this policy store. The
    -- valid values are:
    --
    -- -   __OFF__ – Neither Verified Permissions nor Cedar perform any
    --     validation on policies. No validation errors are reported by either
    --     service.
    --
    -- -   __STRICT__ – Requires a schema to be present in the policy store.
    --     Cedar performs validation on all submitted new or updated static
    --     policies and policy templates. Any that fail validation are rejected
    --     and Cedar doesn\'t store them in the policy store.
    --
    -- If @Mode=STRICT@ and the policy store doesn\'t contain a schema,
    -- Verified Permissions rejects all static policies and policy templates
    -- because there is no schema to validate against.
    --
    -- To submit a static policy or policy template without a schema, you must
    -- turn off validation.
    mode :: ValidationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'validationSettings_mode' - The validation mode currently configured for this policy store. The
-- valid values are:
--
-- -   __OFF__ – Neither Verified Permissions nor Cedar perform any
--     validation on policies. No validation errors are reported by either
--     service.
--
-- -   __STRICT__ – Requires a schema to be present in the policy store.
--     Cedar performs validation on all submitted new or updated static
--     policies and policy templates. Any that fail validation are rejected
--     and Cedar doesn\'t store them in the policy store.
--
-- If @Mode=STRICT@ and the policy store doesn\'t contain a schema,
-- Verified Permissions rejects all static policies and policy templates
-- because there is no schema to validate against.
--
-- To submit a static policy or policy template without a schema, you must
-- turn off validation.
newValidationSettings ::
  -- | 'mode'
  ValidationMode ->
  ValidationSettings
newValidationSettings pMode_ =
  ValidationSettings' {mode = pMode_}

-- | The validation mode currently configured for this policy store. The
-- valid values are:
--
-- -   __OFF__ – Neither Verified Permissions nor Cedar perform any
--     validation on policies. No validation errors are reported by either
--     service.
--
-- -   __STRICT__ – Requires a schema to be present in the policy store.
--     Cedar performs validation on all submitted new or updated static
--     policies and policy templates. Any that fail validation are rejected
--     and Cedar doesn\'t store them in the policy store.
--
-- If @Mode=STRICT@ and the policy store doesn\'t contain a schema,
-- Verified Permissions rejects all static policies and policy templates
-- because there is no schema to validate against.
--
-- To submit a static policy or policy template without a schema, you must
-- turn off validation.
validationSettings_mode :: Lens.Lens' ValidationSettings ValidationMode
validationSettings_mode = Lens.lens (\ValidationSettings' {mode} -> mode) (\s@ValidationSettings' {} a -> s {mode = a} :: ValidationSettings)

instance Data.FromJSON ValidationSettings where
  parseJSON =
    Data.withObject
      "ValidationSettings"
      ( \x ->
          ValidationSettings' Prelude.<$> (x Data..: "mode")
      )

instance Prelude.Hashable ValidationSettings where
  hashWithSalt _salt ValidationSettings' {..} =
    _salt `Prelude.hashWithSalt` mode

instance Prelude.NFData ValidationSettings where
  rnf ValidationSettings' {..} = Prelude.rnf mode

instance Data.ToJSON ValidationSettings where
  toJSON ValidationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("mode" Data..= mode)]
      )
