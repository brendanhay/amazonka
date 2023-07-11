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
-- Module      : Amazonka.CloudDirectory.Types.PolicyAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.PolicyAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the @PolicyType@, @PolicyId@, and the @ObjectIdentifier@ to
-- which it is attached. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- /See:/ 'newPolicyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { -- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID of @PolicyAttachment@.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The type of policy that can be associated with @PolicyAttachment@.
    policyType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'policyAttachment_objectIdentifier' - The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
--
-- 'policyId', 'policyAttachment_policyId' - The ID of @PolicyAttachment@.
--
-- 'policyType', 'policyAttachment_policyType' - The type of policy that can be associated with @PolicyAttachment@.
newPolicyAttachment ::
  PolicyAttachment
newPolicyAttachment =
  PolicyAttachment'
    { objectIdentifier =
        Prelude.Nothing,
      policyId = Prelude.Nothing,
      policyType = Prelude.Nothing
    }

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
policyAttachment_objectIdentifier :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_objectIdentifier = Lens.lens (\PolicyAttachment' {objectIdentifier} -> objectIdentifier) (\s@PolicyAttachment' {} a -> s {objectIdentifier = a} :: PolicyAttachment)

-- | The ID of @PolicyAttachment@.
policyAttachment_policyId :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_policyId = Lens.lens (\PolicyAttachment' {policyId} -> policyId) (\s@PolicyAttachment' {} a -> s {policyId = a} :: PolicyAttachment)

-- | The type of policy that can be associated with @PolicyAttachment@.
policyAttachment_policyType :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_policyType = Lens.lens (\PolicyAttachment' {policyType} -> policyType) (\s@PolicyAttachment' {} a -> s {policyType = a} :: PolicyAttachment)

instance Data.FromJSON PolicyAttachment where
  parseJSON =
    Data.withObject
      "PolicyAttachment"
      ( \x ->
          PolicyAttachment'
            Prelude.<$> (x Data..:? "ObjectIdentifier")
            Prelude.<*> (x Data..:? "PolicyId")
            Prelude.<*> (x Data..:? "PolicyType")
      )

instance Prelude.Hashable PolicyAttachment where
  hashWithSalt _salt PolicyAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` objectIdentifier
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData PolicyAttachment where
  rnf PolicyAttachment' {..} =
    Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
