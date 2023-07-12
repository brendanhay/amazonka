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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteOptedOutNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing opted out destination phone number from the
-- specified opt-out list.
--
-- Each destination phone number can only be deleted once every 30 days.
--
-- If the specified destination phone number doesn\'t exist or if the
-- opt-out list doesn\'t exist, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.DeleteOptedOutNumber
  ( -- * Creating a Request
    DeleteOptedOutNumber (..),
    newDeleteOptedOutNumber,

    -- * Request Lenses
    deleteOptedOutNumber_optOutListName,
    deleteOptedOutNumber_optedOutNumber,

    -- * Destructuring the Response
    DeleteOptedOutNumberResponse (..),
    newDeleteOptedOutNumberResponse,

    -- * Response Lenses
    deleteOptedOutNumberResponse_endUserOptedOut,
    deleteOptedOutNumberResponse_optOutListArn,
    deleteOptedOutNumberResponse_optOutListName,
    deleteOptedOutNumberResponse_optedOutNumber,
    deleteOptedOutNumberResponse_optedOutTimestamp,
    deleteOptedOutNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOptedOutNumber' smart constructor.
data DeleteOptedOutNumber = DeleteOptedOutNumber'
  { -- | The OptOutListName or OptOutListArn to remove the phone number from.
    optOutListName :: Prelude.Text,
    -- | The phone number, in E.164 format, to remove from the OptOutList.
    optedOutNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptedOutNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optOutListName', 'deleteOptedOutNumber_optOutListName' - The OptOutListName or OptOutListArn to remove the phone number from.
--
-- 'optedOutNumber', 'deleteOptedOutNumber_optedOutNumber' - The phone number, in E.164 format, to remove from the OptOutList.
newDeleteOptedOutNumber ::
  -- | 'optOutListName'
  Prelude.Text ->
  -- | 'optedOutNumber'
  Prelude.Text ->
  DeleteOptedOutNumber
newDeleteOptedOutNumber
  pOptOutListName_
  pOptedOutNumber_ =
    DeleteOptedOutNumber'
      { optOutListName =
          pOptOutListName_,
        optedOutNumber = pOptedOutNumber_
      }

-- | The OptOutListName or OptOutListArn to remove the phone number from.
deleteOptedOutNumber_optOutListName :: Lens.Lens' DeleteOptedOutNumber Prelude.Text
deleteOptedOutNumber_optOutListName = Lens.lens (\DeleteOptedOutNumber' {optOutListName} -> optOutListName) (\s@DeleteOptedOutNumber' {} a -> s {optOutListName = a} :: DeleteOptedOutNumber)

-- | The phone number, in E.164 format, to remove from the OptOutList.
deleteOptedOutNumber_optedOutNumber :: Lens.Lens' DeleteOptedOutNumber Prelude.Text
deleteOptedOutNumber_optedOutNumber = Lens.lens (\DeleteOptedOutNumber' {optedOutNumber} -> optedOutNumber) (\s@DeleteOptedOutNumber' {} a -> s {optedOutNumber = a} :: DeleteOptedOutNumber)

instance Core.AWSRequest DeleteOptedOutNumber where
  type
    AWSResponse DeleteOptedOutNumber =
      DeleteOptedOutNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOptedOutNumberResponse'
            Prelude.<$> (x Data..?> "EndUserOptedOut")
            Prelude.<*> (x Data..?> "OptOutListArn")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "OptedOutNumber")
            Prelude.<*> (x Data..?> "OptedOutTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOptedOutNumber where
  hashWithSalt _salt DeleteOptedOutNumber' {..} =
    _salt
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` optedOutNumber

instance Prelude.NFData DeleteOptedOutNumber where
  rnf DeleteOptedOutNumber' {..} =
    Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf optedOutNumber

instance Data.ToHeaders DeleteOptedOutNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteOptedOutNumber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteOptedOutNumber where
  toJSON DeleteOptedOutNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OptOutListName" Data..= optOutListName),
            Prelude.Just
              ("OptedOutNumber" Data..= optedOutNumber)
          ]
      )

instance Data.ToPath DeleteOptedOutNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteOptedOutNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOptedOutNumberResponse' smart constructor.
data DeleteOptedOutNumberResponse = DeleteOptedOutNumberResponse'
  { -- | This is true if it was the end user who requested their phone number be
    -- removed.
    endUserOptedOut :: Prelude.Maybe Prelude.Bool,
    -- | The OptOutListArn that the phone number was removed from.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | The OptOutListName that the phone number was removed from.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was removed from the OptOutList.
    optedOutNumber :: Prelude.Maybe Prelude.Text,
    -- | The time that the number was removed at, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    optedOutTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptedOutNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endUserOptedOut', 'deleteOptedOutNumberResponse_endUserOptedOut' - This is true if it was the end user who requested their phone number be
-- removed.
--
-- 'optOutListArn', 'deleteOptedOutNumberResponse_optOutListArn' - The OptOutListArn that the phone number was removed from.
--
-- 'optOutListName', 'deleteOptedOutNumberResponse_optOutListName' - The OptOutListName that the phone number was removed from.
--
-- 'optedOutNumber', 'deleteOptedOutNumberResponse_optedOutNumber' - The phone number that was removed from the OptOutList.
--
-- 'optedOutTimestamp', 'deleteOptedOutNumberResponse_optedOutTimestamp' - The time that the number was removed at, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'httpStatus', 'deleteOptedOutNumberResponse_httpStatus' - The response's http status code.
newDeleteOptedOutNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOptedOutNumberResponse
newDeleteOptedOutNumberResponse pHttpStatus_ =
  DeleteOptedOutNumberResponse'
    { endUserOptedOut =
        Prelude.Nothing,
      optOutListArn = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      optedOutNumber = Prelude.Nothing,
      optedOutTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is true if it was the end user who requested their phone number be
-- removed.
deleteOptedOutNumberResponse_endUserOptedOut :: Lens.Lens' DeleteOptedOutNumberResponse (Prelude.Maybe Prelude.Bool)
deleteOptedOutNumberResponse_endUserOptedOut = Lens.lens (\DeleteOptedOutNumberResponse' {endUserOptedOut} -> endUserOptedOut) (\s@DeleteOptedOutNumberResponse' {} a -> s {endUserOptedOut = a} :: DeleteOptedOutNumberResponse)

-- | The OptOutListArn that the phone number was removed from.
deleteOptedOutNumberResponse_optOutListArn :: Lens.Lens' DeleteOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
deleteOptedOutNumberResponse_optOutListArn = Lens.lens (\DeleteOptedOutNumberResponse' {optOutListArn} -> optOutListArn) (\s@DeleteOptedOutNumberResponse' {} a -> s {optOutListArn = a} :: DeleteOptedOutNumberResponse)

-- | The OptOutListName that the phone number was removed from.
deleteOptedOutNumberResponse_optOutListName :: Lens.Lens' DeleteOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
deleteOptedOutNumberResponse_optOutListName = Lens.lens (\DeleteOptedOutNumberResponse' {optOutListName} -> optOutListName) (\s@DeleteOptedOutNumberResponse' {} a -> s {optOutListName = a} :: DeleteOptedOutNumberResponse)

-- | The phone number that was removed from the OptOutList.
deleteOptedOutNumberResponse_optedOutNumber :: Lens.Lens' DeleteOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
deleteOptedOutNumberResponse_optedOutNumber = Lens.lens (\DeleteOptedOutNumberResponse' {optedOutNumber} -> optedOutNumber) (\s@DeleteOptedOutNumberResponse' {} a -> s {optedOutNumber = a} :: DeleteOptedOutNumberResponse)

-- | The time that the number was removed at, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
deleteOptedOutNumberResponse_optedOutTimestamp :: Lens.Lens' DeleteOptedOutNumberResponse (Prelude.Maybe Prelude.UTCTime)
deleteOptedOutNumberResponse_optedOutTimestamp = Lens.lens (\DeleteOptedOutNumberResponse' {optedOutTimestamp} -> optedOutTimestamp) (\s@DeleteOptedOutNumberResponse' {} a -> s {optedOutTimestamp = a} :: DeleteOptedOutNumberResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
deleteOptedOutNumberResponse_httpStatus :: Lens.Lens' DeleteOptedOutNumberResponse Prelude.Int
deleteOptedOutNumberResponse_httpStatus = Lens.lens (\DeleteOptedOutNumberResponse' {httpStatus} -> httpStatus) (\s@DeleteOptedOutNumberResponse' {} a -> s {httpStatus = a} :: DeleteOptedOutNumberResponse)

instance Prelude.NFData DeleteOptedOutNumberResponse where
  rnf DeleteOptedOutNumberResponse' {..} =
    Prelude.rnf endUserOptedOut
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf optedOutNumber
      `Prelude.seq` Prelude.rnf optedOutTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
