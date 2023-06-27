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
-- Module      : Amazonka.CloudFront.DeleteFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption configuration.
module Amazonka.CloudFront.DeleteFieldLevelEncryptionConfig
  ( -- * Creating a Request
    DeleteFieldLevelEncryptionConfig (..),
    newDeleteFieldLevelEncryptionConfig,

    -- * Request Lenses
    deleteFieldLevelEncryptionConfig_ifMatch,
    deleteFieldLevelEncryptionConfig_id,

    -- * Destructuring the Response
    DeleteFieldLevelEncryptionConfigResponse (..),
    newDeleteFieldLevelEncryptionConfigResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFieldLevelEncryptionConfig' smart constructor.
data DeleteFieldLevelEncryptionConfig = DeleteFieldLevelEncryptionConfig'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- configuration identity to delete. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The ID of the configuration you want to delete from CloudFront.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteFieldLevelEncryptionConfig_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- configuration identity to delete. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deleteFieldLevelEncryptionConfig_id' - The ID of the configuration you want to delete from CloudFront.
newDeleteFieldLevelEncryptionConfig ::
  -- | 'id'
  Prelude.Text ->
  DeleteFieldLevelEncryptionConfig
newDeleteFieldLevelEncryptionConfig pId_ =
  DeleteFieldLevelEncryptionConfig'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- configuration identity to delete. For example: @E2QWRUHAPOMQZL@.
deleteFieldLevelEncryptionConfig_ifMatch :: Lens.Lens' DeleteFieldLevelEncryptionConfig (Prelude.Maybe Prelude.Text)
deleteFieldLevelEncryptionConfig_ifMatch = Lens.lens (\DeleteFieldLevelEncryptionConfig' {ifMatch} -> ifMatch) (\s@DeleteFieldLevelEncryptionConfig' {} a -> s {ifMatch = a} :: DeleteFieldLevelEncryptionConfig)

-- | The ID of the configuration you want to delete from CloudFront.
deleteFieldLevelEncryptionConfig_id :: Lens.Lens' DeleteFieldLevelEncryptionConfig Prelude.Text
deleteFieldLevelEncryptionConfig_id = Lens.lens (\DeleteFieldLevelEncryptionConfig' {id} -> id) (\s@DeleteFieldLevelEncryptionConfig' {} a -> s {id = a} :: DeleteFieldLevelEncryptionConfig)

instance
  Core.AWSRequest
    DeleteFieldLevelEncryptionConfig
  where
  type
    AWSResponse DeleteFieldLevelEncryptionConfig =
      DeleteFieldLevelEncryptionConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFieldLevelEncryptionConfigResponse'

instance
  Prelude.Hashable
    DeleteFieldLevelEncryptionConfig
  where
  hashWithSalt
    _salt
    DeleteFieldLevelEncryptionConfig' {..} =
      _salt
        `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteFieldLevelEncryptionConfig
  where
  rnf DeleteFieldLevelEncryptionConfig' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteFieldLevelEncryptionConfig
  where
  toHeaders DeleteFieldLevelEncryptionConfig' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteFieldLevelEncryptionConfig where
  toPath DeleteFieldLevelEncryptionConfig' {..} =
    Prelude.mconcat
      ["/2020-05-31/field-level-encryption/", Data.toBS id]

instance
  Data.ToQuery
    DeleteFieldLevelEncryptionConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFieldLevelEncryptionConfigResponse' smart constructor.
data DeleteFieldLevelEncryptionConfigResponse = DeleteFieldLevelEncryptionConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFieldLevelEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFieldLevelEncryptionConfigResponse ::
  DeleteFieldLevelEncryptionConfigResponse
newDeleteFieldLevelEncryptionConfigResponse =
  DeleteFieldLevelEncryptionConfigResponse'

instance
  Prelude.NFData
    DeleteFieldLevelEncryptionConfigResponse
  where
  rnf _ = ()
