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
-- Module      : Network.AWS.EMR.DeleteStudio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an Amazon EMR Studio from the Studio metadata store.
module Network.AWS.EMR.DeleteStudio
  ( -- * Creating a Request
    DeleteStudio (..),
    newDeleteStudio,

    -- * Request Lenses
    deleteStudio_studioId,

    -- * Destructuring the Response
    DeleteStudioResponse (..),
    newDeleteStudioResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStudio' smart constructor.
data DeleteStudio = DeleteStudio'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'deleteStudio_studioId' - The ID of the Amazon EMR Studio.
newDeleteStudio ::
  -- | 'studioId'
  Prelude.Text ->
  DeleteStudio
newDeleteStudio pStudioId_ =
  DeleteStudio' {studioId = pStudioId_}

-- | The ID of the Amazon EMR Studio.
deleteStudio_studioId :: Lens.Lens' DeleteStudio Prelude.Text
deleteStudio_studioId = Lens.lens (\DeleteStudio' {studioId} -> studioId) (\s@DeleteStudio' {} a -> s {studioId = a} :: DeleteStudio)

instance Prelude.AWSRequest DeleteStudio where
  type Rs DeleteStudio = DeleteStudioResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteStudioResponse'

instance Prelude.Hashable DeleteStudio

instance Prelude.NFData DeleteStudio

instance Prelude.ToHeaders DeleteStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.DeleteStudio" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteStudio where
  toJSON DeleteStudio' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("StudioId" Prelude..= studioId)]
      )

instance Prelude.ToPath DeleteStudio where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStudioResponse ::
  DeleteStudioResponse
newDeleteStudioResponse = DeleteStudioResponse'

instance Prelude.NFData DeleteStudioResponse
