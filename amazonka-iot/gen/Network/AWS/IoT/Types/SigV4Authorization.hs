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
-- Module      : Network.AWS.IoT.Types.SigV4Authorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigV4Authorization where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 signing process>.
--
-- /See:/ 'newSigV4Authorization' smart constructor.
data SigV4Authorization = SigV4Authorization'
  { -- | The signing region.
    signingRegion :: Prelude.Text,
    -- | The service name to use while signing with Sig V4.
    serviceName :: Prelude.Text,
    -- | The ARN of the signing role.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SigV4Authorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingRegion', 'sigV4Authorization_signingRegion' - The signing region.
--
-- 'serviceName', 'sigV4Authorization_serviceName' - The service name to use while signing with Sig V4.
--
-- 'roleArn', 'sigV4Authorization_roleArn' - The ARN of the signing role.
newSigV4Authorization ::
  -- | 'signingRegion'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  SigV4Authorization
newSigV4Authorization
  pSigningRegion_
  pServiceName_
  pRoleArn_ =
    SigV4Authorization'
      { signingRegion =
          pSigningRegion_,
        serviceName = pServiceName_,
        roleArn = pRoleArn_
      }

-- | The signing region.
sigV4Authorization_signingRegion :: Lens.Lens' SigV4Authorization Prelude.Text
sigV4Authorization_signingRegion = Lens.lens (\SigV4Authorization' {signingRegion} -> signingRegion) (\s@SigV4Authorization' {} a -> s {signingRegion = a} :: SigV4Authorization)

-- | The service name to use while signing with Sig V4.
sigV4Authorization_serviceName :: Lens.Lens' SigV4Authorization Prelude.Text
sigV4Authorization_serviceName = Lens.lens (\SigV4Authorization' {serviceName} -> serviceName) (\s@SigV4Authorization' {} a -> s {serviceName = a} :: SigV4Authorization)

-- | The ARN of the signing role.
sigV4Authorization_roleArn :: Lens.Lens' SigV4Authorization Prelude.Text
sigV4Authorization_roleArn = Lens.lens (\SigV4Authorization' {roleArn} -> roleArn) (\s@SigV4Authorization' {} a -> s {roleArn = a} :: SigV4Authorization)

instance Prelude.FromJSON SigV4Authorization where
  parseJSON =
    Prelude.withObject
      "SigV4Authorization"
      ( \x ->
          SigV4Authorization'
            Prelude.<$> (x Prelude..: "signingRegion")
            Prelude.<*> (x Prelude..: "serviceName")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance Prelude.Hashable SigV4Authorization

instance Prelude.NFData SigV4Authorization

instance Prelude.ToJSON SigV4Authorization where
  toJSON SigV4Authorization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("signingRegion" Prelude..= signingRegion),
            Prelude.Just ("serviceName" Prelude..= serviceName),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
