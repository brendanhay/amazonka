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
-- Module      : Network.AWS.CodeGuruReviewer.Types.KMSKeyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types.KMSKeyDetails where

import Network.AWS.CodeGuruReviewer.Types.EncryptionOption
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains:
--
-- -   The encryption option for a repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with a
--     respository association.
--
-- /See:/ 'newKMSKeyDetails' smart constructor.
data KMSKeyDetails = KMSKeyDetails'
  { -- | The encryption option for a repository association. It is either owned
    -- by Amazon Web Services Key Management Service (KMS) (@AWS_OWNED_CMK@) or
    -- customer managed (@CUSTOMER_MANAGED_CMK@).
    encryptionOption :: Prelude.Maybe EncryptionOption,
    -- | The ID of the Amazon Web Services KMS key that is associated with a
    -- respository association.
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
-- respository association.
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
-- respository association.
kmsKeyDetails_kmsKeyId :: Lens.Lens' KMSKeyDetails (Prelude.Maybe Prelude.Text)
kmsKeyDetails_kmsKeyId = Lens.lens (\KMSKeyDetails' {kmsKeyId} -> kmsKeyId) (\s@KMSKeyDetails' {} a -> s {kmsKeyId = a} :: KMSKeyDetails)

instance Core.FromJSON KMSKeyDetails where
  parseJSON =
    Core.withObject
      "KMSKeyDetails"
      ( \x ->
          KMSKeyDetails'
            Prelude.<$> (x Core..:? "EncryptionOption")
            Prelude.<*> (x Core..:? "KMSKeyId")
      )

instance Prelude.Hashable KMSKeyDetails

instance Prelude.NFData KMSKeyDetails

instance Core.ToJSON KMSKeyDetails where
  toJSON KMSKeyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionOption" Core..=)
              Prelude.<$> encryptionOption,
            ("KMSKeyId" Core..=) Prelude.<$> kmsKeyId
          ]
      )
