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
-- Module      : Amazonka.SecurityLake.Types.AwsIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AwsIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The AWS identity.
--
-- /See:/ 'newAwsIdentity' smart constructor.
data AwsIdentity = AwsIdentity'
  { -- | The external ID used to estalish trust relationship with the AWS
    -- identity.
    externalId :: Prelude.Text,
    -- | The AWS identity principal.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'awsIdentity_externalId' - The external ID used to estalish trust relationship with the AWS
-- identity.
--
-- 'principal', 'awsIdentity_principal' - The AWS identity principal.
newAwsIdentity ::
  -- | 'externalId'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  AwsIdentity
newAwsIdentity pExternalId_ pPrincipal_ =
  AwsIdentity'
    { externalId = pExternalId_,
      principal = pPrincipal_
    }

-- | The external ID used to estalish trust relationship with the AWS
-- identity.
awsIdentity_externalId :: Lens.Lens' AwsIdentity Prelude.Text
awsIdentity_externalId = Lens.lens (\AwsIdentity' {externalId} -> externalId) (\s@AwsIdentity' {} a -> s {externalId = a} :: AwsIdentity)

-- | The AWS identity principal.
awsIdentity_principal :: Lens.Lens' AwsIdentity Prelude.Text
awsIdentity_principal = Lens.lens (\AwsIdentity' {principal} -> principal) (\s@AwsIdentity' {} a -> s {principal = a} :: AwsIdentity)

instance Data.FromJSON AwsIdentity where
  parseJSON =
    Data.withObject
      "AwsIdentity"
      ( \x ->
          AwsIdentity'
            Prelude.<$> (x Data..: "externalId")
            Prelude.<*> (x Data..: "principal")
      )

instance Prelude.Hashable AwsIdentity where
  hashWithSalt _salt AwsIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` principal

instance Prelude.NFData AwsIdentity where
  rnf AwsIdentity' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf principal

instance Data.ToJSON AwsIdentity where
  toJSON AwsIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("externalId" Data..= externalId),
            Prelude.Just ("principal" Data..= principal)
          ]
      )
