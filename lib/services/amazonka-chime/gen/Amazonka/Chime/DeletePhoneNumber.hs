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
-- Module      : Amazonka.Chime.DeletePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified phone number into the __Deletion queue__. A phone
-- number must be disassociated from any users or Amazon Chime Voice
-- Connectors before it can be deleted.
--
-- Deleted phone numbers remain in the __Deletion queue__ for 7 days before
-- they are deleted permanently.
module Amazonka.Chime.DeletePhoneNumber
  ( -- * Creating a Request
    DeletePhoneNumber (..),
    newDeletePhoneNumber,

    -- * Request Lenses
    deletePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    DeletePhoneNumberResponse (..),
    newDeletePhoneNumberResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePhoneNumber' smart constructor.
data DeletePhoneNumber = DeletePhoneNumber'
  { -- | The phone number ID.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'deletePhoneNumber_phoneNumberId' - The phone number ID.
newDeletePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  DeletePhoneNumber
newDeletePhoneNumber pPhoneNumberId_ =
  DeletePhoneNumber' {phoneNumberId = pPhoneNumberId_}

-- | The phone number ID.
deletePhoneNumber_phoneNumberId :: Lens.Lens' DeletePhoneNumber Prelude.Text
deletePhoneNumber_phoneNumberId = Lens.lens (\DeletePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@DeletePhoneNumber' {} a -> s {phoneNumberId = a} :: DeletePhoneNumber)

instance Core.AWSRequest DeletePhoneNumber where
  type
    AWSResponse DeletePhoneNumber =
      DeletePhoneNumberResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeletePhoneNumberResponse'

instance Prelude.Hashable DeletePhoneNumber where
  hashWithSalt _salt DeletePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData DeletePhoneNumber where
  rnf DeletePhoneNumber' {..} =
    Prelude.rnf phoneNumberId

instance Data.ToHeaders DeletePhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePhoneNumber where
  toPath DeletePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-numbers/", Data.toBS phoneNumberId]

instance Data.ToQuery DeletePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePhoneNumberResponse' smart constructor.
data DeletePhoneNumberResponse = DeletePhoneNumberResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePhoneNumberResponse ::
  DeletePhoneNumberResponse
newDeletePhoneNumberResponse =
  DeletePhoneNumberResponse'

instance Prelude.NFData DeletePhoneNumberResponse where
  rnf _ = ()
