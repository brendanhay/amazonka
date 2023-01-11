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
-- Module      : Amazonka.Signer.Types.SigningMaterial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningMaterial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ACM certificate that is used to sign your code.
--
-- /See:/ 'newSigningMaterial' smart constructor.
data SigningMaterial = SigningMaterial'
  { -- | The Amazon Resource Name (ARN) of the certificates that is used to sign
    -- your code.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'signingMaterial_certificateArn' - The Amazon Resource Name (ARN) of the certificates that is used to sign
-- your code.
newSigningMaterial ::
  -- | 'certificateArn'
  Prelude.Text ->
  SigningMaterial
newSigningMaterial pCertificateArn_ =
  SigningMaterial' {certificateArn = pCertificateArn_}

-- | The Amazon Resource Name (ARN) of the certificates that is used to sign
-- your code.
signingMaterial_certificateArn :: Lens.Lens' SigningMaterial Prelude.Text
signingMaterial_certificateArn = Lens.lens (\SigningMaterial' {certificateArn} -> certificateArn) (\s@SigningMaterial' {} a -> s {certificateArn = a} :: SigningMaterial)

instance Data.FromJSON SigningMaterial where
  parseJSON =
    Data.withObject
      "SigningMaterial"
      ( \x ->
          SigningMaterial'
            Prelude.<$> (x Data..: "certificateArn")
      )

instance Prelude.Hashable SigningMaterial where
  hashWithSalt _salt SigningMaterial' {..} =
    _salt `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData SigningMaterial where
  rnf SigningMaterial' {..} = Prelude.rnf certificateArn

instance Data.ToJSON SigningMaterial where
  toJSON SigningMaterial' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateArn" Data..= certificateArn)
          ]
      )
