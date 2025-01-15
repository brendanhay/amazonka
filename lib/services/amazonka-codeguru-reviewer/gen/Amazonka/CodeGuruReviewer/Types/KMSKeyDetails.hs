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
-- Module      : Amazonka.CodeGuruReviewer.Types.KMSKeyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.KMSKeyDetails where

import Amazonka.CodeGuruReviewer.Types.EncryptionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains:
--
-- -   The encryption option for a repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with a
--     repository association.
--
-- /See:/ 'newKMSKeyDetails' smart constructor.
data KMSKeyDetails = KMSKeyDetails'
  { -- | The encryption option for a repository association. It is either owned
    -- by Amazon Web Services Key Management Service (KMS) (@AWS_OWNED_CMK@) or
    -- customer managed (@CUSTOMER_MANAGED_CMK@).
    encryptionOption :: Prelude.Maybe EncryptionOption,
    -- | The ID of the Amazon Web Services KMS key that is associated with a
    -- repository association.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KMSKeyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionOption', 'kmsKeyDetails_encryptionOption' - The encryption option for a repository association. It is either owned
-- by Amazon Web Services Key Management Service (KMS) (@AWS_OWNED_CMK@) or
-- customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- 'kmsKeyId', 'kmsKeyDetails_kmsKeyId' - The ID of the Amazon Web Services KMS key that is associated with a
-- repository association.
newKMSKeyDetails ::
  KMSKeyDetails
newKMSKeyDetails =
  KMSKeyDetails'
    { encryptionOption = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | The encryption option for a repository association. It is either owned
-- by Amazon Web Services Key Management Service (KMS) (@AWS_OWNED_CMK@) or
-- customer managed (@CUSTOMER_MANAGED_CMK@).
kmsKeyDetails_encryptionOption :: Lens.Lens' KMSKeyDetails (Prelude.Maybe EncryptionOption)
kmsKeyDetails_encryptionOption = Lens.lens (\KMSKeyDetails' {encryptionOption} -> encryptionOption) (\s@KMSKeyDetails' {} a -> s {encryptionOption = a} :: KMSKeyDetails)

-- | The ID of the Amazon Web Services KMS key that is associated with a
-- repository association.
kmsKeyDetails_kmsKeyId :: Lens.Lens' KMSKeyDetails (Prelude.Maybe Prelude.Text)
kmsKeyDetails_kmsKeyId = Lens.lens (\KMSKeyDetails' {kmsKeyId} -> kmsKeyId) (\s@KMSKeyDetails' {} a -> s {kmsKeyId = a} :: KMSKeyDetails)

instance Data.FromJSON KMSKeyDetails where
  parseJSON =
    Data.withObject
      "KMSKeyDetails"
      ( \x ->
          KMSKeyDetails'
            Prelude.<$> (x Data..:? "EncryptionOption")
            Prelude.<*> (x Data..:? "KMSKeyId")
      )

instance Prelude.Hashable KMSKeyDetails where
  hashWithSalt _salt KMSKeyDetails' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData KMSKeyDetails where
  rnf KMSKeyDetails' {..} =
    Prelude.rnf encryptionOption `Prelude.seq`
      Prelude.rnf kmsKeyId

instance Data.ToJSON KMSKeyDetails where
  toJSON KMSKeyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionOption" Data..=)
              Prelude.<$> encryptionOption,
            ("KMSKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
