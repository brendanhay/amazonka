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
-- Module      : Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment\'s upcoming and in-progress managed actions.
module Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActions
  ( -- * Creating a Request
    DescribeEnvironmentManagedActions (..),
    newDescribeEnvironmentManagedActions,

    -- * Request Lenses
    describeEnvironmentManagedActions_environmentName,
    describeEnvironmentManagedActions_status,
    describeEnvironmentManagedActions_environmentId,

    -- * Destructuring the Response
    DescribeEnvironmentManagedActionsResponse (..),
    newDescribeEnvironmentManagedActionsResponse,

    -- * Response Lenses
    describeEnvironmentManagedActionsResponse_managedActions,
    describeEnvironmentManagedActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to list an environment\'s upcoming and in-progress managed
-- actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActions' smart constructor.
data DescribeEnvironmentManagedActions = DescribeEnvironmentManagedActions'
  { -- | The name of the target environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | To show only actions with a particular status, specify a status.
    status :: Prelude.Maybe ActionStatus,
    -- | The environment ID of the target environment.
    environmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentManagedActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'describeEnvironmentManagedActions_environmentName' - The name of the target environment.
--
-- 'status', 'describeEnvironmentManagedActions_status' - To show only actions with a particular status, specify a status.
--
-- 'environmentId', 'describeEnvironmentManagedActions_environmentId' - The environment ID of the target environment.
newDescribeEnvironmentManagedActions ::
  DescribeEnvironmentManagedActions
newDescribeEnvironmentManagedActions =
  DescribeEnvironmentManagedActions'
    { environmentName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      environmentId = Prelude.Nothing
    }

-- | The name of the target environment.
describeEnvironmentManagedActions_environmentName :: Lens.Lens' DescribeEnvironmentManagedActions (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActions_environmentName = Lens.lens (\DescribeEnvironmentManagedActions' {environmentName} -> environmentName) (\s@DescribeEnvironmentManagedActions' {} a -> s {environmentName = a} :: DescribeEnvironmentManagedActions)

-- | To show only actions with a particular status, specify a status.
describeEnvironmentManagedActions_status :: Lens.Lens' DescribeEnvironmentManagedActions (Prelude.Maybe ActionStatus)
describeEnvironmentManagedActions_status = Lens.lens (\DescribeEnvironmentManagedActions' {status} -> status) (\s@DescribeEnvironmentManagedActions' {} a -> s {status = a} :: DescribeEnvironmentManagedActions)

-- | The environment ID of the target environment.
describeEnvironmentManagedActions_environmentId :: Lens.Lens' DescribeEnvironmentManagedActions (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActions_environmentId = Lens.lens (\DescribeEnvironmentManagedActions' {environmentId} -> environmentId) (\s@DescribeEnvironmentManagedActions' {} a -> s {environmentId = a} :: DescribeEnvironmentManagedActions)

instance
  Core.AWSRequest
    DescribeEnvironmentManagedActions
  where
  type
    AWSResponse DescribeEnvironmentManagedActions =
      DescribeEnvironmentManagedActionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentManagedActionsResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionsResponse'
            Prelude.<$> ( x Core..@? "ManagedActions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList1 "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEnvironmentManagedActions
  where
  hashWithSalt
    _salt
    DescribeEnvironmentManagedActions' {..} =
      _salt `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` environmentId

instance
  Prelude.NFData
    DescribeEnvironmentManagedActions
  where
  rnf DescribeEnvironmentManagedActions' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf environmentId

instance
  Core.ToHeaders
    DescribeEnvironmentManagedActions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeEnvironmentManagedActions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeEnvironmentManagedActions
  where
  toQuery DescribeEnvironmentManagedActions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeEnvironmentManagedActions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentName" Core.=: environmentName,
        "Status" Core.=: status,
        "EnvironmentId" Core.=: environmentId
      ]

-- | The result message containing a list of managed actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActionsResponse' smart constructor.
data DescribeEnvironmentManagedActionsResponse = DescribeEnvironmentManagedActionsResponse'
  { -- | A list of upcoming and in-progress managed actions.
    managedActions :: Prelude.Maybe (Prelude.NonEmpty ManagedAction),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentManagedActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedActions', 'describeEnvironmentManagedActionsResponse_managedActions' - A list of upcoming and in-progress managed actions.
--
-- 'httpStatus', 'describeEnvironmentManagedActionsResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentManagedActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEnvironmentManagedActionsResponse
newDescribeEnvironmentManagedActionsResponse
  pHttpStatus_ =
    DescribeEnvironmentManagedActionsResponse'
      { managedActions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of upcoming and in-progress managed actions.
describeEnvironmentManagedActionsResponse_managedActions :: Lens.Lens' DescribeEnvironmentManagedActionsResponse (Prelude.Maybe (Prelude.NonEmpty ManagedAction))
describeEnvironmentManagedActionsResponse_managedActions = Lens.lens (\DescribeEnvironmentManagedActionsResponse' {managedActions} -> managedActions) (\s@DescribeEnvironmentManagedActionsResponse' {} a -> s {managedActions = a} :: DescribeEnvironmentManagedActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEnvironmentManagedActionsResponse_httpStatus :: Lens.Lens' DescribeEnvironmentManagedActionsResponse Prelude.Int
describeEnvironmentManagedActionsResponse_httpStatus = Lens.lens (\DescribeEnvironmentManagedActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentManagedActionsResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentManagedActionsResponse)

instance
  Prelude.NFData
    DescribeEnvironmentManagedActionsResponse
  where
  rnf DescribeEnvironmentManagedActionsResponse' {..} =
    Prelude.rnf managedActions
      `Prelude.seq` Prelude.rnf httpStatus
