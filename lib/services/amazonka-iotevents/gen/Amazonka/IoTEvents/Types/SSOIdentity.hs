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
-- Module      : Amazonka.IoTEvents.Types.SSOIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SSOIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about your identity source in AWS Single Sign-On.
-- For more information, see the
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html AWS Single Sign-On User Guide>.
--
-- /See:/ 'newSSOIdentity' smart constructor.
data SSOIdentity = SSOIdentity'
  { -- | The user ID.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS SSO identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSOIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'sSOIdentity_userId' - The user ID.
--
-- 'identityStoreId', 'sSOIdentity_identityStoreId' - The ID of the AWS SSO identity store.
newSSOIdentity ::
  -- | 'identityStoreId'
  Prelude.Text ->
  SSOIdentity
newSSOIdentity pIdentityStoreId_ =
  SSOIdentity'
    { userId = Prelude.Nothing,
      identityStoreId = pIdentityStoreId_
    }

-- | The user ID.
sSOIdentity_userId :: Lens.Lens' SSOIdentity (Prelude.Maybe Prelude.Text)
sSOIdentity_userId = Lens.lens (\SSOIdentity' {userId} -> userId) (\s@SSOIdentity' {} a -> s {userId = a} :: SSOIdentity)

-- | The ID of the AWS SSO identity store.
sSOIdentity_identityStoreId :: Lens.Lens' SSOIdentity Prelude.Text
sSOIdentity_identityStoreId = Lens.lens (\SSOIdentity' {identityStoreId} -> identityStoreId) (\s@SSOIdentity' {} a -> s {identityStoreId = a} :: SSOIdentity)

instance Data.FromJSON SSOIdentity where
  parseJSON =
    Data.withObject
      "SSOIdentity"
      ( \x ->
          SSOIdentity'
            Prelude.<$> (x Data..:? "userId")
            Prelude.<*> (x Data..: "identityStoreId")
      )

instance Prelude.Hashable SSOIdentity where
  hashWithSalt _salt SSOIdentity' {..} =
    _salt `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData SSOIdentity where
  rnf SSOIdentity' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf identityStoreId

instance Data.ToJSON SSOIdentity where
  toJSON SSOIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("userId" Data..=) Prelude.<$> userId,
            Prelude.Just
              ("identityStoreId" Data..= identityStoreId)
          ]
      )
