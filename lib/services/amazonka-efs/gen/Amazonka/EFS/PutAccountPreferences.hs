{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.PutAccountPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to set the account preference in the current Amazon
-- Web Services Region to use long 17 character (63 bit) or short 8
-- character (32 bit) resource IDs for new EFS file system and mount target
-- resources. All existing resource IDs are not affected by any changes you
-- make. You can set the ID preference during the opt-in period as EFS
-- transitions to long resource IDs. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/manage-efs-resource-ids.html Managing Amazon EFS resource IDs>.
--
-- Starting in October, 2021, you will receive an error if you try to set
-- the account preference to use the short 8 character format resource ID.
-- Contact Amazon Web Services support if you receive an error and must use
-- short IDs for file system and mount target resources.
module Amazonka.EFS.PutAccountPreferences
  ( -- * Creating a Request
    PutAccountPreferences (..),
    newPutAccountPreferences,

    -- * Request Lenses
    putAccountPreferences_resourceIdType,

    -- * Destructuring the Response
    PutAccountPreferencesResponse (..),
    newPutAccountPreferencesResponse,

    -- * Response Lenses
    putAccountPreferencesResponse_resourceIdPreference,
    putAccountPreferencesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAccountPreferences' smart constructor.
data PutAccountPreferences = PutAccountPreferences'
  { -- | Specifies the EFS resource ID preference to set for the user\'s Amazon
    -- Web Services account, in the current Amazon Web Services Region, either
    -- @LONG_ID@ (17 characters), or @SHORT_ID@ (8 characters).
    --
    -- Starting in October, 2021, you will receive an error when setting the
    -- account preference to @SHORT_ID@. Contact Amazon Web Services support if
    -- you receive an error and must use short IDs for file system and mount
    -- target resources.
    resourceIdType :: ResourceIdType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdType', 'putAccountPreferences_resourceIdType' - Specifies the EFS resource ID preference to set for the user\'s Amazon
-- Web Services account, in the current Amazon Web Services Region, either
-- @LONG_ID@ (17 characters), or @SHORT_ID@ (8 characters).
--
-- Starting in October, 2021, you will receive an error when setting the
-- account preference to @SHORT_ID@. Contact Amazon Web Services support if
-- you receive an error and must use short IDs for file system and mount
-- target resources.
newPutAccountPreferences ::
  -- | 'resourceIdType'
  ResourceIdType ->
  PutAccountPreferences
newPutAccountPreferences pResourceIdType_ =
  PutAccountPreferences'
    { resourceIdType =
        pResourceIdType_
    }

-- | Specifies the EFS resource ID preference to set for the user\'s Amazon
-- Web Services account, in the current Amazon Web Services Region, either
-- @LONG_ID@ (17 characters), or @SHORT_ID@ (8 characters).
--
-- Starting in October, 2021, you will receive an error when setting the
-- account preference to @SHORT_ID@. Contact Amazon Web Services support if
-- you receive an error and must use short IDs for file system and mount
-- target resources.
putAccountPreferences_resourceIdType :: Lens.Lens' PutAccountPreferences ResourceIdType
putAccountPreferences_resourceIdType = Lens.lens (\PutAccountPreferences' {resourceIdType} -> resourceIdType) (\s@PutAccountPreferences' {} a -> s {resourceIdType = a} :: PutAccountPreferences)

instance Core.AWSRequest PutAccountPreferences where
  type
    AWSResponse PutAccountPreferences =
      PutAccountPreferencesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountPreferencesResponse'
            Prelude.<$> (x Core..?> "ResourceIdPreference")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountPreferences where
  hashWithSalt _salt PutAccountPreferences' {..} =
    _salt `Prelude.hashWithSalt` resourceIdType

instance Prelude.NFData PutAccountPreferences where
  rnf PutAccountPreferences' {..} =
    Prelude.rnf resourceIdType

instance Core.ToHeaders PutAccountPreferences where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutAccountPreferences where
  toJSON PutAccountPreferences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceIdType" Core..= resourceIdType)
          ]
      )

instance Core.ToPath PutAccountPreferences where
  toPath =
    Prelude.const "/2015-02-01/account-preferences"

instance Core.ToQuery PutAccountPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountPreferencesResponse' smart constructor.
data PutAccountPreferencesResponse = PutAccountPreferencesResponse'
  { resourceIdPreference :: Prelude.Maybe ResourceIdPreference,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdPreference', 'putAccountPreferencesResponse_resourceIdPreference' - Undocumented member.
--
-- 'httpStatus', 'putAccountPreferencesResponse_httpStatus' - The response's http status code.
newPutAccountPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountPreferencesResponse
newPutAccountPreferencesResponse pHttpStatus_ =
  PutAccountPreferencesResponse'
    { resourceIdPreference =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putAccountPreferencesResponse_resourceIdPreference :: Lens.Lens' PutAccountPreferencesResponse (Prelude.Maybe ResourceIdPreference)
putAccountPreferencesResponse_resourceIdPreference = Lens.lens (\PutAccountPreferencesResponse' {resourceIdPreference} -> resourceIdPreference) (\s@PutAccountPreferencesResponse' {} a -> s {resourceIdPreference = a} :: PutAccountPreferencesResponse)

-- | The response's http status code.
putAccountPreferencesResponse_httpStatus :: Lens.Lens' PutAccountPreferencesResponse Prelude.Int
putAccountPreferencesResponse_httpStatus = Lens.lens (\PutAccountPreferencesResponse' {httpStatus} -> httpStatus) (\s@PutAccountPreferencesResponse' {} a -> s {httpStatus = a} :: PutAccountPreferencesResponse)

instance Prelude.NFData PutAccountPreferencesResponse where
  rnf PutAccountPreferencesResponse' {..} =
    Prelude.rnf resourceIdPreference
      `Prelude.seq` Prelude.rnf httpStatus
