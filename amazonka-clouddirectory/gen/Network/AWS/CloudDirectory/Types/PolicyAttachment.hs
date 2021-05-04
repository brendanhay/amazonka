{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.Types.PolicyAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PolicyAttachment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the @PolicyType@, @PolicyId@, and the @ObjectIdentifier@ to
-- which it is attached. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- /See:/ 'newPolicyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { -- | The type of policy that can be associated with @PolicyAttachment@.
    policyType :: Prelude.Maybe Prelude.Text,
    -- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID of @PolicyAttachment@.
    policyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyType', 'policyAttachment_policyType' - The type of policy that can be associated with @PolicyAttachment@.
--
-- 'objectIdentifier', 'policyAttachment_objectIdentifier' - The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
--
-- 'policyId', 'policyAttachment_policyId' - The ID of @PolicyAttachment@.
newPolicyAttachment ::
  PolicyAttachment
newPolicyAttachment =
  PolicyAttachment'
    { policyType = Prelude.Nothing,
      objectIdentifier = Prelude.Nothing,
      policyId = Prelude.Nothing
    }

-- | The type of policy that can be associated with @PolicyAttachment@.
policyAttachment_policyType :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_policyType = Lens.lens (\PolicyAttachment' {policyType} -> policyType) (\s@PolicyAttachment' {} a -> s {policyType = a} :: PolicyAttachment)

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@.
policyAttachment_objectIdentifier :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_objectIdentifier = Lens.lens (\PolicyAttachment' {objectIdentifier} -> objectIdentifier) (\s@PolicyAttachment' {} a -> s {objectIdentifier = a} :: PolicyAttachment)

-- | The ID of @PolicyAttachment@.
policyAttachment_policyId :: Lens.Lens' PolicyAttachment (Prelude.Maybe Prelude.Text)
policyAttachment_policyId = Lens.lens (\PolicyAttachment' {policyId} -> policyId) (\s@PolicyAttachment' {} a -> s {policyId = a} :: PolicyAttachment)

instance Prelude.FromJSON PolicyAttachment where
  parseJSON =
    Prelude.withObject
      "PolicyAttachment"
      ( \x ->
          PolicyAttachment'
            Prelude.<$> (x Prelude..:? "PolicyType")
            Prelude.<*> (x Prelude..:? "ObjectIdentifier")
            Prelude.<*> (x Prelude..:? "PolicyId")
      )

instance Prelude.Hashable PolicyAttachment

instance Prelude.NFData PolicyAttachment
