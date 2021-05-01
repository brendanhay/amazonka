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
-- Module      : Network.AWS.Translate.DeleteTerminology
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A synchronous action that deletes a custom terminology.
module Network.AWS.Translate.DeleteTerminology
  ( -- * Creating a Request
    DeleteTerminology (..),
    newDeleteTerminology,

    -- * Request Lenses
    deleteTerminology_name,

    -- * Destructuring the Response
    DeleteTerminologyResponse (..),
    newDeleteTerminologyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newDeleteTerminology' smart constructor.
data DeleteTerminology = DeleteTerminology'
  { -- | The name of the custom terminology being deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteTerminology_name' - The name of the custom terminology being deleted.
newDeleteTerminology ::
  -- | 'name'
  Prelude.Text ->
  DeleteTerminology
newDeleteTerminology pName_ =
  DeleteTerminology' {name = pName_}

-- | The name of the custom terminology being deleted.
deleteTerminology_name :: Lens.Lens' DeleteTerminology Prelude.Text
deleteTerminology_name = Lens.lens (\DeleteTerminology' {name} -> name) (\s@DeleteTerminology' {} a -> s {name = a} :: DeleteTerminology)

instance Prelude.AWSRequest DeleteTerminology where
  type Rs DeleteTerminology = DeleteTerminologyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteTerminologyResponse'

instance Prelude.Hashable DeleteTerminology

instance Prelude.NFData DeleteTerminology

instance Prelude.ToHeaders DeleteTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShineFrontendService_20170701.DeleteTerminology" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTerminology where
  toJSON DeleteTerminology' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteTerminology where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTerminologyResponse' smart constructor.
data DeleteTerminologyResponse = DeleteTerminologyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTerminologyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTerminologyResponse ::
  DeleteTerminologyResponse
newDeleteTerminologyResponse =
  DeleteTerminologyResponse'

instance Prelude.NFData DeleteTerminologyResponse
