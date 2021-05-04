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
-- Module      : Network.AWS.LexModels.DeleteIntent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.LexModels.DeleteIntent
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIntent' smart constructor.
data DeleteIntent = DeleteIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteIntent where
  type Rs DeleteIntent = DeleteIntentResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteIntentResponse'

instance Prelude.Hashable DeleteIntent

instance Prelude.NFData DeleteIntent

instance Prelude.ToHeaders DeleteIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteIntent where
  toPath DeleteIntent' {..} =
    Prelude.mconcat ["/intents/", Prelude.toBS name]

instance Prelude.ToQuery DeleteIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntentResponse' smart constructor.
data DeleteIntentResponse = DeleteIntentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentResponse ::
  DeleteIntentResponse
newDeleteIntentResponse = DeleteIntentResponse'

instance Prelude.NFData DeleteIntentResponse
