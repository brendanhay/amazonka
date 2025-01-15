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
-- Module      : Amazonka.Scheduler.GetScheduleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified schedule group.
module Amazonka.Scheduler.GetScheduleGroup
  ( -- * Creating a Request
    GetScheduleGroup (..),
    newGetScheduleGroup,

    -- * Request Lenses
    getScheduleGroup_name,

    -- * Destructuring the Response
    GetScheduleGroupResponse (..),
    newGetScheduleGroupResponse,

    -- * Response Lenses
    getScheduleGroupResponse_arn,
    getScheduleGroupResponse_creationDate,
    getScheduleGroupResponse_lastModificationDate,
    getScheduleGroupResponse_name,
    getScheduleGroupResponse_state,
    getScheduleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newGetScheduleGroup' smart constructor.
data GetScheduleGroup = GetScheduleGroup'
  { -- | The name of the schedule group to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScheduleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getScheduleGroup_name' - The name of the schedule group to retrieve.
newGetScheduleGroup ::
  -- | 'name'
  Prelude.Text ->
  GetScheduleGroup
newGetScheduleGroup pName_ =
  GetScheduleGroup' {name = pName_}

-- | The name of the schedule group to retrieve.
getScheduleGroup_name :: Lens.Lens' GetScheduleGroup Prelude.Text
getScheduleGroup_name = Lens.lens (\GetScheduleGroup' {name} -> name) (\s@GetScheduleGroup' {} a -> s {name = a} :: GetScheduleGroup)

instance Core.AWSRequest GetScheduleGroup where
  type
    AWSResponse GetScheduleGroup =
      GetScheduleGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetScheduleGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "LastModificationDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetScheduleGroup where
  hashWithSalt _salt GetScheduleGroup' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetScheduleGroup where
  rnf GetScheduleGroup' {..} = Prelude.rnf name

instance Data.ToHeaders GetScheduleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetScheduleGroup where
  toPath GetScheduleGroup' {..} =
    Prelude.mconcat
      ["/schedule-groups/", Data.toBS name]

instance Data.ToQuery GetScheduleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetScheduleGroupResponse' smart constructor.
data GetScheduleGroupResponse = GetScheduleGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the schedule group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the schedule group was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The time at which the schedule group was last modified.
    lastModificationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the schedule group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the state of the schedule group.
    state :: Prelude.Maybe ScheduleGroupState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScheduleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getScheduleGroupResponse_arn' - The Amazon Resource Name (ARN) of the schedule group.
--
-- 'creationDate', 'getScheduleGroupResponse_creationDate' - The time at which the schedule group was created.
--
-- 'lastModificationDate', 'getScheduleGroupResponse_lastModificationDate' - The time at which the schedule group was last modified.
--
-- 'name', 'getScheduleGroupResponse_name' - The name of the schedule group.
--
-- 'state', 'getScheduleGroupResponse_state' - Specifies the state of the schedule group.
--
-- 'httpStatus', 'getScheduleGroupResponse_httpStatus' - The response's http status code.
newGetScheduleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetScheduleGroupResponse
newGetScheduleGroupResponse pHttpStatus_ =
  GetScheduleGroupResponse'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModificationDate = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schedule group.
getScheduleGroupResponse_arn :: Lens.Lens' GetScheduleGroupResponse (Prelude.Maybe Prelude.Text)
getScheduleGroupResponse_arn = Lens.lens (\GetScheduleGroupResponse' {arn} -> arn) (\s@GetScheduleGroupResponse' {} a -> s {arn = a} :: GetScheduleGroupResponse)

-- | The time at which the schedule group was created.
getScheduleGroupResponse_creationDate :: Lens.Lens' GetScheduleGroupResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleGroupResponse_creationDate = Lens.lens (\GetScheduleGroupResponse' {creationDate} -> creationDate) (\s@GetScheduleGroupResponse' {} a -> s {creationDate = a} :: GetScheduleGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The time at which the schedule group was last modified.
getScheduleGroupResponse_lastModificationDate :: Lens.Lens' GetScheduleGroupResponse (Prelude.Maybe Prelude.UTCTime)
getScheduleGroupResponse_lastModificationDate = Lens.lens (\GetScheduleGroupResponse' {lastModificationDate} -> lastModificationDate) (\s@GetScheduleGroupResponse' {} a -> s {lastModificationDate = a} :: GetScheduleGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the schedule group.
getScheduleGroupResponse_name :: Lens.Lens' GetScheduleGroupResponse (Prelude.Maybe Prelude.Text)
getScheduleGroupResponse_name = Lens.lens (\GetScheduleGroupResponse' {name} -> name) (\s@GetScheduleGroupResponse' {} a -> s {name = a} :: GetScheduleGroupResponse)

-- | Specifies the state of the schedule group.
getScheduleGroupResponse_state :: Lens.Lens' GetScheduleGroupResponse (Prelude.Maybe ScheduleGroupState)
getScheduleGroupResponse_state = Lens.lens (\GetScheduleGroupResponse' {state} -> state) (\s@GetScheduleGroupResponse' {} a -> s {state = a} :: GetScheduleGroupResponse)

-- | The response's http status code.
getScheduleGroupResponse_httpStatus :: Lens.Lens' GetScheduleGroupResponse Prelude.Int
getScheduleGroupResponse_httpStatus = Lens.lens (\GetScheduleGroupResponse' {httpStatus} -> httpStatus) (\s@GetScheduleGroupResponse' {} a -> s {httpStatus = a} :: GetScheduleGroupResponse)

instance Prelude.NFData GetScheduleGroupResponse where
  rnf GetScheduleGroupResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationDate `Prelude.seq`
        Prelude.rnf lastModificationDate `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf httpStatus
