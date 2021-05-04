{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption profile.
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
  ( -- * Creating a Request
    DeleteFieldLevelEncryptionProfile (..),
    newDeleteFieldLevelEncryptionProfile,

    -- * Request Lenses
    deleteFieldLevelEncryptionProfile_ifMatch,
    deleteFieldLevelEncryptionProfile_id,

    -- * Destructuring the Response
    DeleteFieldLevelEncryptionProfileResponse (..),
    newDeleteFieldLevelEncryptionProfileResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFieldLevelEncryptionProfile' smart constructor.
data DeleteFieldLevelEncryptionProfile = DeleteFieldLevelEncryptionProfile'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- profile to delete. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | Request the ID of the profile you want to delete from CloudFront.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFieldLevelEncryptionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteFieldLevelEncryptionProfile_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- profile to delete. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deleteFieldLevelEncryptionProfile_id' - Request the ID of the profile you want to delete from CloudFront.
newDeleteFieldLevelEncryptionProfile ::
  -- | 'id'
  Prelude.Text ->
  DeleteFieldLevelEncryptionProfile
newDeleteFieldLevelEncryptionProfile pId_ =
  DeleteFieldLevelEncryptionProfile'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- profile to delete. For example: @E2QWRUHAPOMQZL@.
deleteFieldLevelEncryptionProfile_ifMatch :: Lens.Lens' DeleteFieldLevelEncryptionProfile (Prelude.Maybe Prelude.Text)
deleteFieldLevelEncryptionProfile_ifMatch = Lens.lens (\DeleteFieldLevelEncryptionProfile' {ifMatch} -> ifMatch) (\s@DeleteFieldLevelEncryptionProfile' {} a -> s {ifMatch = a} :: DeleteFieldLevelEncryptionProfile)

-- | Request the ID of the profile you want to delete from CloudFront.
deleteFieldLevelEncryptionProfile_id :: Lens.Lens' DeleteFieldLevelEncryptionProfile Prelude.Text
deleteFieldLevelEncryptionProfile_id = Lens.lens (\DeleteFieldLevelEncryptionProfile' {id} -> id) (\s@DeleteFieldLevelEncryptionProfile' {} a -> s {id = a} :: DeleteFieldLevelEncryptionProfile)

instance
  Prelude.AWSRequest
    DeleteFieldLevelEncryptionProfile
  where
  type
    Rs DeleteFieldLevelEncryptionProfile =
      DeleteFieldLevelEncryptionProfileResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteFieldLevelEncryptionProfileResponse'

instance
  Prelude.Hashable
    DeleteFieldLevelEncryptionProfile

instance
  Prelude.NFData
    DeleteFieldLevelEncryptionProfile

instance
  Prelude.ToHeaders
    DeleteFieldLevelEncryptionProfile
  where
  toHeaders DeleteFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance
  Prelude.ToPath
    DeleteFieldLevelEncryptionProfile
  where
  toPath DeleteFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Prelude.toBS id
      ]

instance
  Prelude.ToQuery
    DeleteFieldLevelEncryptionProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFieldLevelEncryptionProfileResponse' smart constructor.
data DeleteFieldLevelEncryptionProfileResponse = DeleteFieldLevelEncryptionProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFieldLevelEncryptionProfileResponse ::
  DeleteFieldLevelEncryptionProfileResponse
newDeleteFieldLevelEncryptionProfileResponse =
  DeleteFieldLevelEncryptionProfileResponse'

instance
  Prelude.NFData
    DeleteFieldLevelEncryptionProfileResponse
