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
-- Module      : Amazonka.VerifiedPermissions.Types.SchemaDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.SchemaDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of principal types, resource types, and actions that can
-- be specified in policies stored in the same policy store. If the
-- validation mode for the policy store is set to @STRICT@, then policies
-- that can\'t be validated by this schema are rejected by Verified
-- Permissions and can\'t be stored in the policy store.
--
-- /See:/ 'newSchemaDefinition' smart constructor.
data SchemaDefinition = SchemaDefinition'
  { -- | A JSON string representation of the schema supported by applications
    -- that use this policy store. For more information, see
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/userguide/schema.html Policy store schema>
    -- in the /Amazon Verified Permissions User Guide/.
    cedarJson :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cedarJson', 'schemaDefinition_cedarJson' - A JSON string representation of the schema supported by applications
-- that use this policy store. For more information, see
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/userguide/schema.html Policy store schema>
-- in the /Amazon Verified Permissions User Guide/.
newSchemaDefinition ::
  SchemaDefinition
newSchemaDefinition =
  SchemaDefinition' {cedarJson = Prelude.Nothing}

-- | A JSON string representation of the schema supported by applications
-- that use this policy store. For more information, see
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/userguide/schema.html Policy store schema>
-- in the /Amazon Verified Permissions User Guide/.
schemaDefinition_cedarJson :: Lens.Lens' SchemaDefinition (Prelude.Maybe Prelude.Text)
schemaDefinition_cedarJson = Lens.lens (\SchemaDefinition' {cedarJson} -> cedarJson) (\s@SchemaDefinition' {} a -> s {cedarJson = a} :: SchemaDefinition)

instance Prelude.Hashable SchemaDefinition where
  hashWithSalt _salt SchemaDefinition' {..} =
    _salt `Prelude.hashWithSalt` cedarJson

instance Prelude.NFData SchemaDefinition where
  rnf SchemaDefinition' {..} = Prelude.rnf cedarJson

instance Data.ToJSON SchemaDefinition where
  toJSON SchemaDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("cedarJson" Data..=) Prelude.<$> cedarJson]
      )
