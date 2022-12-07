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
-- Module      : Amazonka.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information
-- such as the create date, the last updated date, membership information,
-- and the work team\'s Amazon Resource Name (ARN).
module Amazonka.SageMaker.DescribeWorkteam
  ( -- * Creating a Request
    DescribeWorkteam (..),
    newDescribeWorkteam,

    -- * Request Lenses
    describeWorkteam_workteamName,

    -- * Destructuring the Response
    DescribeWorkteamResponse (..),
    newDescribeWorkteamResponse,

    -- * Response Lenses
    describeWorkteamResponse_httpStatus,
    describeWorkteamResponse_workteam,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeWorkteam' smart constructor.
data DescribeWorkteam = DescribeWorkteam'
  { -- | The name of the work team to return a description of.
    workteamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamName', 'describeWorkteam_workteamName' - The name of the work team to return a description of.
newDescribeWorkteam ::
  -- | 'workteamName'
  Prelude.Text ->
  DescribeWorkteam
newDescribeWorkteam pWorkteamName_ =
  DescribeWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to return a description of.
describeWorkteam_workteamName :: Lens.Lens' DescribeWorkteam Prelude.Text
describeWorkteam_workteamName = Lens.lens (\DescribeWorkteam' {workteamName} -> workteamName) (\s@DescribeWorkteam' {} a -> s {workteamName = a} :: DescribeWorkteam)

instance Core.AWSRequest DescribeWorkteam where
  type
    AWSResponse DescribeWorkteam =
      DescribeWorkteamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkteamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Workteam")
      )

instance Prelude.Hashable DescribeWorkteam where
  hashWithSalt _salt DescribeWorkteam' {..} =
    _salt `Prelude.hashWithSalt` workteamName

instance Prelude.NFData DescribeWorkteam where
  rnf DescribeWorkteam' {..} = Prelude.rnf workteamName

instance Data.ToHeaders DescribeWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeWorkteam" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWorkteam where
  toJSON DescribeWorkteam' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkteamName" Data..= workteamName)]
      )

instance Data.ToPath DescribeWorkteam where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @Workteam@ instance that contains information about the work team.
    workteam :: Workteam
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'workteam', 'describeWorkteamResponse_workteam' - A @Workteam@ instance that contains information about the work team.
newDescribeWorkteamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workteam'
  Workteam ->
  DescribeWorkteamResponse
newDescribeWorkteamResponse pHttpStatus_ pWorkteam_ =
  DescribeWorkteamResponse'
    { httpStatus =
        pHttpStatus_,
      workteam = pWorkteam_
    }

-- | The response's http status code.
describeWorkteamResponse_httpStatus :: Lens.Lens' DescribeWorkteamResponse Prelude.Int
describeWorkteamResponse_httpStatus = Lens.lens (\DescribeWorkteamResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkteamResponse' {} a -> s {httpStatus = a} :: DescribeWorkteamResponse)

-- | A @Workteam@ instance that contains information about the work team.
describeWorkteamResponse_workteam :: Lens.Lens' DescribeWorkteamResponse Workteam
describeWorkteamResponse_workteam = Lens.lens (\DescribeWorkteamResponse' {workteam} -> workteam) (\s@DescribeWorkteamResponse' {} a -> s {workteam = a} :: DescribeWorkteamResponse)

instance Prelude.NFData DescribeWorkteamResponse where
  rnf DescribeWorkteamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workteam
