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
-- Module      : Amazonka.SageMaker.DeleteHubContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the contents of a hub.
module Amazonka.SageMaker.DeleteHubContent
  ( -- * Creating a Request
    DeleteHubContent (..),
    newDeleteHubContent,

    -- * Request Lenses
    deleteHubContent_hubName,
    deleteHubContent_hubContentType,
    deleteHubContent_hubContentName,
    deleteHubContent_hubContentVersion,

    -- * Destructuring the Response
    DeleteHubContentResponse (..),
    newDeleteHubContentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteHubContent' smart constructor.
data DeleteHubContent = DeleteHubContent'
  { -- | The name of the hub that you want to delete content in.
    hubName :: Prelude.Text,
    -- | The type of content that you want to delete from a hub.
    hubContentType :: HubContentType,
    -- | The name of the content that you want to delete from a hub.
    hubContentName :: Prelude.Text,
    -- | The version of the content that you want to delete from a hub.
    hubContentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHubContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubName', 'deleteHubContent_hubName' - The name of the hub that you want to delete content in.
--
-- 'hubContentType', 'deleteHubContent_hubContentType' - The type of content that you want to delete from a hub.
--
-- 'hubContentName', 'deleteHubContent_hubContentName' - The name of the content that you want to delete from a hub.
--
-- 'hubContentVersion', 'deleteHubContent_hubContentVersion' - The version of the content that you want to delete from a hub.
newDeleteHubContent ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'hubContentName'
  Prelude.Text ->
  -- | 'hubContentVersion'
  Prelude.Text ->
  DeleteHubContent
newDeleteHubContent
  pHubName_
  pHubContentType_
  pHubContentName_
  pHubContentVersion_ =
    DeleteHubContent'
      { hubName = pHubName_,
        hubContentType = pHubContentType_,
        hubContentName = pHubContentName_,
        hubContentVersion = pHubContentVersion_
      }

-- | The name of the hub that you want to delete content in.
deleteHubContent_hubName :: Lens.Lens' DeleteHubContent Prelude.Text
deleteHubContent_hubName = Lens.lens (\DeleteHubContent' {hubName} -> hubName) (\s@DeleteHubContent' {} a -> s {hubName = a} :: DeleteHubContent)

-- | The type of content that you want to delete from a hub.
deleteHubContent_hubContentType :: Lens.Lens' DeleteHubContent HubContentType
deleteHubContent_hubContentType = Lens.lens (\DeleteHubContent' {hubContentType} -> hubContentType) (\s@DeleteHubContent' {} a -> s {hubContentType = a} :: DeleteHubContent)

-- | The name of the content that you want to delete from a hub.
deleteHubContent_hubContentName :: Lens.Lens' DeleteHubContent Prelude.Text
deleteHubContent_hubContentName = Lens.lens (\DeleteHubContent' {hubContentName} -> hubContentName) (\s@DeleteHubContent' {} a -> s {hubContentName = a} :: DeleteHubContent)

-- | The version of the content that you want to delete from a hub.
deleteHubContent_hubContentVersion :: Lens.Lens' DeleteHubContent Prelude.Text
deleteHubContent_hubContentVersion = Lens.lens (\DeleteHubContent' {hubContentVersion} -> hubContentVersion) (\s@DeleteHubContent' {} a -> s {hubContentVersion = a} :: DeleteHubContent)

instance Core.AWSRequest DeleteHubContent where
  type
    AWSResponse DeleteHubContent =
      DeleteHubContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteHubContentResponse'

instance Prelude.Hashable DeleteHubContent where
  hashWithSalt _salt DeleteHubContent' {..} =
    _salt `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubContentType
      `Prelude.hashWithSalt` hubContentName
      `Prelude.hashWithSalt` hubContentVersion

instance Prelude.NFData DeleteHubContent where
  rnf DeleteHubContent' {..} =
    Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubContentType
      `Prelude.seq` Prelude.rnf hubContentName
      `Prelude.seq` Prelude.rnf hubContentVersion

instance Data.ToHeaders DeleteHubContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteHubContent" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHubContent where
  toJSON DeleteHubContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubContentType" Data..= hubContentType),
            Prelude.Just
              ("HubContentName" Data..= hubContentName),
            Prelude.Just
              ("HubContentVersion" Data..= hubContentVersion)
          ]
      )

instance Data.ToPath DeleteHubContent where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHubContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHubContentResponse' smart constructor.
data DeleteHubContentResponse = DeleteHubContentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHubContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHubContentResponse ::
  DeleteHubContentResponse
newDeleteHubContentResponse =
  DeleteHubContentResponse'

instance Prelude.NFData DeleteHubContentResponse where
  rnf _ = ()
