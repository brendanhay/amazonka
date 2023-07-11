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
-- Module      : Amazonka.LexModels.DeleteIntent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the intent, including the @$LATEST@ version. To
-- delete a specific version of the intent, use the DeleteIntentVersion
-- operation.
--
-- You can delete a version of an intent only if it is not referenced. To
-- delete an intent that is referred to in one or more bots (see
-- how-it-works), you must remove those references first.
--
-- If you get the @ResourceInUseException@ exception, it provides an
-- example reference that shows where the intent is referenced. To remove
-- the reference to the intent, either update the bot or delete it. If you
-- get the same exception when you attempt to delete the intent again,
-- repeat until the intent has no references and the call to @DeleteIntent@
-- is successful.
--
-- This operation requires permission for the @lex:DeleteIntent@ action.
module Amazonka.LexModels.DeleteIntent
  ( -- * Creating a Request
    DeleteIntent (..),
    newDeleteIntent,

    -- * Request Lenses
    deleteIntent_name,

    -- * Destructuring the Response
    DeleteIntentResponse (..),
    newDeleteIntentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIntent' smart constructor.
data DeleteIntent = DeleteIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteIntent_name' - The name of the intent. The name is case sensitive.
newDeleteIntent ::
  -- | 'name'
  Prelude.Text ->
  DeleteIntent
newDeleteIntent pName_ = DeleteIntent' {name = pName_}

-- | The name of the intent. The name is case sensitive.
deleteIntent_name :: Lens.Lens' DeleteIntent Prelude.Text
deleteIntent_name = Lens.lens (\DeleteIntent' {name} -> name) (\s@DeleteIntent' {} a -> s {name = a} :: DeleteIntent)

instance Core.AWSRequest DeleteIntent where
  type AWSResponse DeleteIntent = DeleteIntentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteIntentResponse'

instance Prelude.Hashable DeleteIntent where
  hashWithSalt _salt DeleteIntent' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteIntent where
  rnf DeleteIntent' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIntent where
  toPath DeleteIntent' {..} =
    Prelude.mconcat ["/intents/", Data.toBS name]

instance Data.ToQuery DeleteIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntentResponse' smart constructor.
data DeleteIntentResponse = DeleteIntentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentResponse ::
  DeleteIntentResponse
newDeleteIntentResponse = DeleteIntentResponse'

instance Prelude.NFData DeleteIntentResponse where
  rnf _ = ()
