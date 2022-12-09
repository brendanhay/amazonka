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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteOptOutList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing opt-out list. All opted out phone numbers in the
-- opt-out list are deleted.
--
-- If the specified opt-out list name doesn\'t exist or is in-use by an
-- origination phone number or pool, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.DeleteOptOutList
  ( -- * Creating a Request
    DeleteOptOutList (..),
    newDeleteOptOutList,

    -- * Request Lenses
    deleteOptOutList_optOutListName,

    -- * Destructuring the Response
    DeleteOptOutListResponse (..),
    newDeleteOptOutListResponse,

    -- * Response Lenses
    deleteOptOutListResponse_createdTimestamp,
    deleteOptOutListResponse_optOutListArn,
    deleteOptOutListResponse_optOutListName,
    deleteOptOutListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOptOutList' smart constructor.
data DeleteOptOutList = DeleteOptOutList'
  { -- | The OptOutListName or OptOutListArn of the OptOutList to delete. You can
    -- use DescribeOptOutLists to find the values for OptOutListName and
    -- OptOutListArn.
    optOutListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptOutList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optOutListName', 'deleteOptOutList_optOutListName' - The OptOutListName or OptOutListArn of the OptOutList to delete. You can
-- use DescribeOptOutLists to find the values for OptOutListName and
-- OptOutListArn.
newDeleteOptOutList ::
  -- | 'optOutListName'
  Prelude.Text ->
  DeleteOptOutList
newDeleteOptOutList pOptOutListName_ =
  DeleteOptOutList'
    { optOutListName =
        pOptOutListName_
    }

-- | The OptOutListName or OptOutListArn of the OptOutList to delete. You can
-- use DescribeOptOutLists to find the values for OptOutListName and
-- OptOutListArn.
deleteOptOutList_optOutListName :: Lens.Lens' DeleteOptOutList Prelude.Text
deleteOptOutList_optOutListName = Lens.lens (\DeleteOptOutList' {optOutListName} -> optOutListName) (\s@DeleteOptOutList' {} a -> s {optOutListName = a} :: DeleteOptOutList)

instance Core.AWSRequest DeleteOptOutList where
  type
    AWSResponse DeleteOptOutList =
      DeleteOptOutListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOptOutListResponse'
            Prelude.<$> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "OptOutListArn")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOptOutList where
  hashWithSalt _salt DeleteOptOutList' {..} =
    _salt `Prelude.hashWithSalt` optOutListName

instance Prelude.NFData DeleteOptOutList where
  rnf DeleteOptOutList' {..} =
    Prelude.rnf optOutListName

instance Data.ToHeaders DeleteOptOutList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteOptOutList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteOptOutList where
  toJSON DeleteOptOutList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OptOutListName" Data..= optOutListName)
          ]
      )

instance Data.ToPath DeleteOptOutList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteOptOutList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOptOutListResponse' smart constructor.
data DeleteOptOutListResponse = DeleteOptOutListResponse'
  { -- | The time when the OptOutList was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the OptOutList that was removed.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the OptOutList that was removed.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptOutListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'deleteOptOutListResponse_createdTimestamp' - The time when the OptOutList was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'optOutListArn', 'deleteOptOutListResponse_optOutListArn' - The Amazon Resource Name (ARN) of the OptOutList that was removed.
--
-- 'optOutListName', 'deleteOptOutListResponse_optOutListName' - The name of the OptOutList that was removed.
--
-- 'httpStatus', 'deleteOptOutListResponse_httpStatus' - The response's http status code.
newDeleteOptOutListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOptOutListResponse
newDeleteOptOutListResponse pHttpStatus_ =
  DeleteOptOutListResponse'
    { createdTimestamp =
        Prelude.Nothing,
      optOutListArn = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the OptOutList was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
deleteOptOutListResponse_createdTimestamp :: Lens.Lens' DeleteOptOutListResponse (Prelude.Maybe Prelude.UTCTime)
deleteOptOutListResponse_createdTimestamp = Lens.lens (\DeleteOptOutListResponse' {createdTimestamp} -> createdTimestamp) (\s@DeleteOptOutListResponse' {} a -> s {createdTimestamp = a} :: DeleteOptOutListResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the OptOutList that was removed.
deleteOptOutListResponse_optOutListArn :: Lens.Lens' DeleteOptOutListResponse (Prelude.Maybe Prelude.Text)
deleteOptOutListResponse_optOutListArn = Lens.lens (\DeleteOptOutListResponse' {optOutListArn} -> optOutListArn) (\s@DeleteOptOutListResponse' {} a -> s {optOutListArn = a} :: DeleteOptOutListResponse)

-- | The name of the OptOutList that was removed.
deleteOptOutListResponse_optOutListName :: Lens.Lens' DeleteOptOutListResponse (Prelude.Maybe Prelude.Text)
deleteOptOutListResponse_optOutListName = Lens.lens (\DeleteOptOutListResponse' {optOutListName} -> optOutListName) (\s@DeleteOptOutListResponse' {} a -> s {optOutListName = a} :: DeleteOptOutListResponse)

-- | The response's http status code.
deleteOptOutListResponse_httpStatus :: Lens.Lens' DeleteOptOutListResponse Prelude.Int
deleteOptOutListResponse_httpStatus = Lens.lens (\DeleteOptOutListResponse' {httpStatus} -> httpStatus) (\s@DeleteOptOutListResponse' {} a -> s {httpStatus = a} :: DeleteOptOutListResponse)

instance Prelude.NFData DeleteOptOutListResponse where
  rnf DeleteOptOutListResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf httpStatus
