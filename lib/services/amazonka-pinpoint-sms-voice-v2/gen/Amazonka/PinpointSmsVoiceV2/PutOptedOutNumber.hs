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
-- Module      : Amazonka.PinpointSmsVoiceV2.PutOptedOutNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an opted out destination phone number in the opt-out list.
--
-- If the destination phone number isn\'t valid or if the specified opt-out
-- list doesn\'t exist, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.PutOptedOutNumber
  ( -- * Creating a Request
    PutOptedOutNumber (..),
    newPutOptedOutNumber,

    -- * Request Lenses
    putOptedOutNumber_optOutListName,
    putOptedOutNumber_optedOutNumber,

    -- * Destructuring the Response
    PutOptedOutNumberResponse (..),
    newPutOptedOutNumberResponse,

    -- * Response Lenses
    putOptedOutNumberResponse_endUserOptedOut,
    putOptedOutNumberResponse_optOutListArn,
    putOptedOutNumberResponse_optOutListName,
    putOptedOutNumberResponse_optedOutNumber,
    putOptedOutNumberResponse_optedOutTimestamp,
    putOptedOutNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOptedOutNumber' smart constructor.
data PutOptedOutNumber = PutOptedOutNumber'
  { -- | The OptOutListName or OptOutListArn to add the phone number to.
    optOutListName :: Prelude.Text,
    -- | The phone number to add to the OptOutList in E.164 format.
    optedOutNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOptedOutNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optOutListName', 'putOptedOutNumber_optOutListName' - The OptOutListName or OptOutListArn to add the phone number to.
--
-- 'optedOutNumber', 'putOptedOutNumber_optedOutNumber' - The phone number to add to the OptOutList in E.164 format.
newPutOptedOutNumber ::
  -- | 'optOutListName'
  Prelude.Text ->
  -- | 'optedOutNumber'
  Prelude.Text ->
  PutOptedOutNumber
newPutOptedOutNumber
  pOptOutListName_
  pOptedOutNumber_ =
    PutOptedOutNumber'
      { optOutListName =
          pOptOutListName_,
        optedOutNumber = pOptedOutNumber_
      }

-- | The OptOutListName or OptOutListArn to add the phone number to.
putOptedOutNumber_optOutListName :: Lens.Lens' PutOptedOutNumber Prelude.Text
putOptedOutNumber_optOutListName = Lens.lens (\PutOptedOutNumber' {optOutListName} -> optOutListName) (\s@PutOptedOutNumber' {} a -> s {optOutListName = a} :: PutOptedOutNumber)

-- | The phone number to add to the OptOutList in E.164 format.
putOptedOutNumber_optedOutNumber :: Lens.Lens' PutOptedOutNumber Prelude.Text
putOptedOutNumber_optedOutNumber = Lens.lens (\PutOptedOutNumber' {optedOutNumber} -> optedOutNumber) (\s@PutOptedOutNumber' {} a -> s {optedOutNumber = a} :: PutOptedOutNumber)

instance Core.AWSRequest PutOptedOutNumber where
  type
    AWSResponse PutOptedOutNumber =
      PutOptedOutNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOptedOutNumberResponse'
            Prelude.<$> (x Data..?> "EndUserOptedOut")
            Prelude.<*> (x Data..?> "OptOutListArn")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "OptedOutNumber")
            Prelude.<*> (x Data..?> "OptedOutTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutOptedOutNumber where
  hashWithSalt _salt PutOptedOutNumber' {..} =
    _salt
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` optedOutNumber

instance Prelude.NFData PutOptedOutNumber where
  rnf PutOptedOutNumber' {..} =
    Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf optedOutNumber

instance Data.ToHeaders PutOptedOutNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.PutOptedOutNumber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutOptedOutNumber where
  toJSON PutOptedOutNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OptOutListName" Data..= optOutListName),
            Prelude.Just
              ("OptedOutNumber" Data..= optedOutNumber)
          ]
      )

instance Data.ToPath PutOptedOutNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery PutOptedOutNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOptedOutNumberResponse' smart constructor.
data PutOptedOutNumberResponse = PutOptedOutNumberResponse'
  { -- | This is true if it was the end user who requested their phone number be
    -- removed.
    endUserOptedOut :: Prelude.Maybe Prelude.Bool,
    -- | The OptOutListArn that the phone number was removed from.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | The OptOutListName that the phone number was removed from.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was added to the OptOutList.
    optedOutNumber :: Prelude.Maybe Prelude.Text,
    -- | The time that the phone number was added to the OptOutList, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    optedOutTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOptedOutNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endUserOptedOut', 'putOptedOutNumberResponse_endUserOptedOut' - This is true if it was the end user who requested their phone number be
-- removed.
--
-- 'optOutListArn', 'putOptedOutNumberResponse_optOutListArn' - The OptOutListArn that the phone number was removed from.
--
-- 'optOutListName', 'putOptedOutNumberResponse_optOutListName' - The OptOutListName that the phone number was removed from.
--
-- 'optedOutNumber', 'putOptedOutNumberResponse_optedOutNumber' - The phone number that was added to the OptOutList.
--
-- 'optedOutTimestamp', 'putOptedOutNumberResponse_optedOutTimestamp' - The time that the phone number was added to the OptOutList, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'httpStatus', 'putOptedOutNumberResponse_httpStatus' - The response's http status code.
newPutOptedOutNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutOptedOutNumberResponse
newPutOptedOutNumberResponse pHttpStatus_ =
  PutOptedOutNumberResponse'
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
putOptedOutNumberResponse_endUserOptedOut :: Lens.Lens' PutOptedOutNumberResponse (Prelude.Maybe Prelude.Bool)
putOptedOutNumberResponse_endUserOptedOut = Lens.lens (\PutOptedOutNumberResponse' {endUserOptedOut} -> endUserOptedOut) (\s@PutOptedOutNumberResponse' {} a -> s {endUserOptedOut = a} :: PutOptedOutNumberResponse)

-- | The OptOutListArn that the phone number was removed from.
putOptedOutNumberResponse_optOutListArn :: Lens.Lens' PutOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
putOptedOutNumberResponse_optOutListArn = Lens.lens (\PutOptedOutNumberResponse' {optOutListArn} -> optOutListArn) (\s@PutOptedOutNumberResponse' {} a -> s {optOutListArn = a} :: PutOptedOutNumberResponse)

-- | The OptOutListName that the phone number was removed from.
putOptedOutNumberResponse_optOutListName :: Lens.Lens' PutOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
putOptedOutNumberResponse_optOutListName = Lens.lens (\PutOptedOutNumberResponse' {optOutListName} -> optOutListName) (\s@PutOptedOutNumberResponse' {} a -> s {optOutListName = a} :: PutOptedOutNumberResponse)

-- | The phone number that was added to the OptOutList.
putOptedOutNumberResponse_optedOutNumber :: Lens.Lens' PutOptedOutNumberResponse (Prelude.Maybe Prelude.Text)
putOptedOutNumberResponse_optedOutNumber = Lens.lens (\PutOptedOutNumberResponse' {optedOutNumber} -> optedOutNumber) (\s@PutOptedOutNumberResponse' {} a -> s {optedOutNumber = a} :: PutOptedOutNumberResponse)

-- | The time that the phone number was added to the OptOutList, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
putOptedOutNumberResponse_optedOutTimestamp :: Lens.Lens' PutOptedOutNumberResponse (Prelude.Maybe Prelude.UTCTime)
putOptedOutNumberResponse_optedOutTimestamp = Lens.lens (\PutOptedOutNumberResponse' {optedOutTimestamp} -> optedOutTimestamp) (\s@PutOptedOutNumberResponse' {} a -> s {optedOutTimestamp = a} :: PutOptedOutNumberResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
putOptedOutNumberResponse_httpStatus :: Lens.Lens' PutOptedOutNumberResponse Prelude.Int
putOptedOutNumberResponse_httpStatus = Lens.lens (\PutOptedOutNumberResponse' {httpStatus} -> httpStatus) (\s@PutOptedOutNumberResponse' {} a -> s {httpStatus = a} :: PutOptedOutNumberResponse)

instance Prelude.NFData PutOptedOutNumberResponse where
  rnf PutOptedOutNumberResponse' {..} =
    Prelude.rnf endUserOptedOut
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf optedOutNumber
      `Prelude.seq` Prelude.rnf optedOutTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
