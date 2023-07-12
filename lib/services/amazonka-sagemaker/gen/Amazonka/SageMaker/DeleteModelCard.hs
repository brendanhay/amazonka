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
-- Module      : Amazonka.SageMaker.DeleteModelCard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker Model Card.
module Amazonka.SageMaker.DeleteModelCard
  ( -- * Creating a Request
    DeleteModelCard (..),
    newDeleteModelCard,

    -- * Request Lenses
    deleteModelCard_modelCardName,

    -- * Destructuring the Response
    DeleteModelCardResponse (..),
    newDeleteModelCardResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelCard' smart constructor.
data DeleteModelCard = DeleteModelCard'
  { -- | The name of the model card to delete.
    modelCardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCardName', 'deleteModelCard_modelCardName' - The name of the model card to delete.
newDeleteModelCard ::
  -- | 'modelCardName'
  Prelude.Text ->
  DeleteModelCard
newDeleteModelCard pModelCardName_ =
  DeleteModelCard' {modelCardName = pModelCardName_}

-- | The name of the model card to delete.
deleteModelCard_modelCardName :: Lens.Lens' DeleteModelCard Prelude.Text
deleteModelCard_modelCardName = Lens.lens (\DeleteModelCard' {modelCardName} -> modelCardName) (\s@DeleteModelCard' {} a -> s {modelCardName = a} :: DeleteModelCard)

instance Core.AWSRequest DeleteModelCard where
  type
    AWSResponse DeleteModelCard =
      DeleteModelCardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteModelCardResponse'

instance Prelude.Hashable DeleteModelCard where
  hashWithSalt _salt DeleteModelCard' {..} =
    _salt `Prelude.hashWithSalt` modelCardName

instance Prelude.NFData DeleteModelCard where
  rnf DeleteModelCard' {..} = Prelude.rnf modelCardName

instance Data.ToHeaders DeleteModelCard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteModelCard" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteModelCard where
  toJSON DeleteModelCard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelCardName" Data..= modelCardName)
          ]
      )

instance Data.ToPath DeleteModelCard where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteModelCard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelCardResponse' smart constructor.
data DeleteModelCardResponse = DeleteModelCardResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelCardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelCardResponse ::
  DeleteModelCardResponse
newDeleteModelCardResponse = DeleteModelCardResponse'

instance Prelude.NFData DeleteModelCardResponse where
  rnf _ = ()
