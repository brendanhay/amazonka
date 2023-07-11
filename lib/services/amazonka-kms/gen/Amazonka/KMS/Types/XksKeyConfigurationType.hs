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
-- Module      : Amazonka.KMS.Types.XksKeyConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.XksKeyConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/keystore-external.html#concept-external-key external key>
-- that is associated with a KMS key in an external key store.
--
-- This element appears in a CreateKey or DescribeKey response only for a
-- KMS key in an external key store.
--
-- The /external key/ is a symmetric encryption key that is hosted by an
-- external key manager outside of Amazon Web Services. When you use the
-- KMS key in an external key store in a cryptographic operation, the
-- cryptographic operation is performed in the external key manager using
-- the specified external key. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/keystore-external.html#concept-external-key External key>
-- in the /Key Management Service Developer Guide/.
--
-- /See:/ 'newXksKeyConfigurationType' smart constructor.
data XksKeyConfigurationType = XksKeyConfigurationType'
  { -- | The ID of the external key in its external key manager. This is the ID
    -- that the external key store proxy uses to identify the external key.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XksKeyConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'xksKeyConfigurationType_id' - The ID of the external key in its external key manager. This is the ID
-- that the external key store proxy uses to identify the external key.
newXksKeyConfigurationType ::
  XksKeyConfigurationType
newXksKeyConfigurationType =
  XksKeyConfigurationType' {id = Prelude.Nothing}

-- | The ID of the external key in its external key manager. This is the ID
-- that the external key store proxy uses to identify the external key.
xksKeyConfigurationType_id :: Lens.Lens' XksKeyConfigurationType (Prelude.Maybe Prelude.Text)
xksKeyConfigurationType_id = Lens.lens (\XksKeyConfigurationType' {id} -> id) (\s@XksKeyConfigurationType' {} a -> s {id = a} :: XksKeyConfigurationType)

instance Data.FromJSON XksKeyConfigurationType where
  parseJSON =
    Data.withObject
      "XksKeyConfigurationType"
      ( \x ->
          XksKeyConfigurationType'
            Prelude.<$> (x Data..:? "Id")
      )

instance Prelude.Hashable XksKeyConfigurationType where
  hashWithSalt _salt XksKeyConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData XksKeyConfigurationType where
  rnf XksKeyConfigurationType' {..} = Prelude.rnf id
