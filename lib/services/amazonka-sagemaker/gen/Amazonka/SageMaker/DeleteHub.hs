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
-- Module      : Amazonka.SageMaker.DeleteHub
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a hub.
module Amazonka.SageMaker.DeleteHub
  ( -- * Creating a Request
    DeleteHub (..),
    newDeleteHub,

    -- * Request Lenses
    deleteHub_hubName,

    -- * Destructuring the Response
    DeleteHubResponse (..),
    newDeleteHubResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteHub' smart constructor.
data DeleteHub = DeleteHub'
  { -- | The name of the hub to delete.
    hubName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubName', 'deleteHub_hubName' - The name of the hub to delete.
newDeleteHub ::
  -- | 'hubName'
  Prelude.Text ->
  DeleteHub
newDeleteHub pHubName_ =
  DeleteHub' {hubName = pHubName_}

-- | The name of the hub to delete.
deleteHub_hubName :: Lens.Lens' DeleteHub Prelude.Text
deleteHub_hubName = Lens.lens (\DeleteHub' {hubName} -> hubName) (\s@DeleteHub' {} a -> s {hubName = a} :: DeleteHub)

instance Core.AWSRequest DeleteHub where
  type AWSResponse DeleteHub = DeleteHubResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteHubResponse'

instance Prelude.Hashable DeleteHub where
  hashWithSalt _salt DeleteHub' {..} =
    _salt `Prelude.hashWithSalt` hubName

instance Prelude.NFData DeleteHub where
  rnf DeleteHub' {..} = Prelude.rnf hubName

instance Data.ToHeaders DeleteHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteHub" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHub where
  toJSON DeleteHub' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HubName" Data..= hubName)]
      )

instance Data.ToPath DeleteHub where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHub where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHubResponse' smart constructor.
data DeleteHubResponse = DeleteHubResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHubResponse ::
  DeleteHubResponse
newDeleteHubResponse = DeleteHubResponse'

instance Prelude.NFData DeleteHubResponse where
  rnf _ = ()
