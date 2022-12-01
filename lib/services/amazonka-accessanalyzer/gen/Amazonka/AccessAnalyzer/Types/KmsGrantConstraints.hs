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
-- Module      : Amazonka.AccessAnalyzer.Types.KmsGrantConstraints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.KmsGrantConstraints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to propose allowing
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- in the grant only when the operation request includes the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context>.
-- You can specify only one type of encryption context. An empty map is
-- treated as not specified. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_GrantConstraints.html GrantConstraints>.
--
-- /See:/ 'newKmsGrantConstraints' smart constructor.
data KmsGrantConstraints = KmsGrantConstraints'
  { -- | A list of key-value pairs that must be included in the encryption
    -- context of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the cryptographic operation only when the
    -- encryption context in the request includes the key-value pairs specified
    -- in this constraint, although it can include additional key-value pairs.
    encryptionContextSubset :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of key-value pairs that must match the encryption context in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
    -- request. The grant allows the operation only when the encryption context
    -- in the request is the same as the encryption context specified in this
    -- constraint.
    encryptionContextEquals :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsGrantConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionContextSubset', 'kmsGrantConstraints_encryptionContextSubset' - A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
--
-- 'encryptionContextEquals', 'kmsGrantConstraints_encryptionContextEquals' - A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
newKmsGrantConstraints ::
  KmsGrantConstraints
newKmsGrantConstraints =
  KmsGrantConstraints'
    { encryptionContextSubset =
        Prelude.Nothing,
      encryptionContextEquals = Prelude.Nothing
    }

-- | A list of key-value pairs that must be included in the encryption
-- context of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the cryptographic operation only when the
-- encryption context in the request includes the key-value pairs specified
-- in this constraint, although it can include additional key-value pairs.
kmsGrantConstraints_encryptionContextSubset :: Lens.Lens' KmsGrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
kmsGrantConstraints_encryptionContextSubset = Lens.lens (\KmsGrantConstraints' {encryptionContextSubset} -> encryptionContextSubset) (\s@KmsGrantConstraints' {} a -> s {encryptionContextSubset = a} :: KmsGrantConstraints) Prelude.. Lens.mapping Lens.coerced

-- | A list of key-value pairs that must match the encryption context in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operation>
-- request. The grant allows the operation only when the encryption context
-- in the request is the same as the encryption context specified in this
-- constraint.
kmsGrantConstraints_encryptionContextEquals :: Lens.Lens' KmsGrantConstraints (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
kmsGrantConstraints_encryptionContextEquals = Lens.lens (\KmsGrantConstraints' {encryptionContextEquals} -> encryptionContextEquals) (\s@KmsGrantConstraints' {} a -> s {encryptionContextEquals = a} :: KmsGrantConstraints) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON KmsGrantConstraints where
  parseJSON =
    Core.withObject
      "KmsGrantConstraints"
      ( \x ->
          KmsGrantConstraints'
            Prelude.<$> ( x Core..:? "encryptionContextSubset"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "encryptionContextEquals"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable KmsGrantConstraints where
  hashWithSalt _salt KmsGrantConstraints' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionContextSubset
      `Prelude.hashWithSalt` encryptionContextEquals

instance Prelude.NFData KmsGrantConstraints where
  rnf KmsGrantConstraints' {..} =
    Prelude.rnf encryptionContextSubset
      `Prelude.seq` Prelude.rnf encryptionContextEquals

instance Core.ToJSON KmsGrantConstraints where
  toJSON KmsGrantConstraints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionContextSubset" Core..=)
              Prelude.<$> encryptionContextSubset,
            ("encryptionContextEquals" Core..=)
              Prelude.<$> encryptionContextEquals
          ]
      )
