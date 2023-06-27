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
-- Module      : Amazonka.DrS.AssociateSourceNetworkStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a Source Network to an existing CloudFormation Stack and
-- modify launch templates to use this network. Can be used for reverting
-- to previously deployed CloudFormation stacks.
module Amazonka.DrS.AssociateSourceNetworkStack
  ( -- * Creating a Request
    AssociateSourceNetworkStack (..),
    newAssociateSourceNetworkStack,

    -- * Request Lenses
    associateSourceNetworkStack_cfnStackName,
    associateSourceNetworkStack_sourceNetworkID,

    -- * Destructuring the Response
    AssociateSourceNetworkStackResponse (..),
    newAssociateSourceNetworkStackResponse,

    -- * Response Lenses
    associateSourceNetworkStackResponse_job,
    associateSourceNetworkStackResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSourceNetworkStack' smart constructor.
data AssociateSourceNetworkStack = AssociateSourceNetworkStack'
  { -- | CloudFormation template to associate with a Source Network.
    cfnStackName :: Data.Sensitive Prelude.Text,
    -- | The Source Network ID to associate with CloudFormation template.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSourceNetworkStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cfnStackName', 'associateSourceNetworkStack_cfnStackName' - CloudFormation template to associate with a Source Network.
--
-- 'sourceNetworkID', 'associateSourceNetworkStack_sourceNetworkID' - The Source Network ID to associate with CloudFormation template.
newAssociateSourceNetworkStack ::
  -- | 'cfnStackName'
  Prelude.Text ->
  -- | 'sourceNetworkID'
  Prelude.Text ->
  AssociateSourceNetworkStack
newAssociateSourceNetworkStack
  pCfnStackName_
  pSourceNetworkID_ =
    AssociateSourceNetworkStack'
      { cfnStackName =
          Data._Sensitive Lens.# pCfnStackName_,
        sourceNetworkID = pSourceNetworkID_
      }

-- | CloudFormation template to associate with a Source Network.
associateSourceNetworkStack_cfnStackName :: Lens.Lens' AssociateSourceNetworkStack Prelude.Text
associateSourceNetworkStack_cfnStackName = Lens.lens (\AssociateSourceNetworkStack' {cfnStackName} -> cfnStackName) (\s@AssociateSourceNetworkStack' {} a -> s {cfnStackName = a} :: AssociateSourceNetworkStack) Prelude.. Data._Sensitive

-- | The Source Network ID to associate with CloudFormation template.
associateSourceNetworkStack_sourceNetworkID :: Lens.Lens' AssociateSourceNetworkStack Prelude.Text
associateSourceNetworkStack_sourceNetworkID = Lens.lens (\AssociateSourceNetworkStack' {sourceNetworkID} -> sourceNetworkID) (\s@AssociateSourceNetworkStack' {} a -> s {sourceNetworkID = a} :: AssociateSourceNetworkStack)

instance Core.AWSRequest AssociateSourceNetworkStack where
  type
    AWSResponse AssociateSourceNetworkStack =
      AssociateSourceNetworkStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSourceNetworkStackResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSourceNetworkStack where
  hashWithSalt _salt AssociateSourceNetworkStack' {..} =
    _salt
      `Prelude.hashWithSalt` cfnStackName
      `Prelude.hashWithSalt` sourceNetworkID

instance Prelude.NFData AssociateSourceNetworkStack where
  rnf AssociateSourceNetworkStack' {..} =
    Prelude.rnf cfnStackName
      `Prelude.seq` Prelude.rnf sourceNetworkID

instance Data.ToHeaders AssociateSourceNetworkStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSourceNetworkStack where
  toJSON AssociateSourceNetworkStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cfnStackName" Data..= cfnStackName),
            Prelude.Just
              ("sourceNetworkID" Data..= sourceNetworkID)
          ]
      )

instance Data.ToPath AssociateSourceNetworkStack where
  toPath = Prelude.const "/AssociateSourceNetworkStack"

instance Data.ToQuery AssociateSourceNetworkStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSourceNetworkStackResponse' smart constructor.
data AssociateSourceNetworkStackResponse = AssociateSourceNetworkStackResponse'
  { -- | The Source Network association Job.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSourceNetworkStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'associateSourceNetworkStackResponse_job' - The Source Network association Job.
--
-- 'httpStatus', 'associateSourceNetworkStackResponse_httpStatus' - The response's http status code.
newAssociateSourceNetworkStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSourceNetworkStackResponse
newAssociateSourceNetworkStackResponse pHttpStatus_ =
  AssociateSourceNetworkStackResponse'
    { job =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Source Network association Job.
associateSourceNetworkStackResponse_job :: Lens.Lens' AssociateSourceNetworkStackResponse (Prelude.Maybe Job)
associateSourceNetworkStackResponse_job = Lens.lens (\AssociateSourceNetworkStackResponse' {job} -> job) (\s@AssociateSourceNetworkStackResponse' {} a -> s {job = a} :: AssociateSourceNetworkStackResponse)

-- | The response's http status code.
associateSourceNetworkStackResponse_httpStatus :: Lens.Lens' AssociateSourceNetworkStackResponse Prelude.Int
associateSourceNetworkStackResponse_httpStatus = Lens.lens (\AssociateSourceNetworkStackResponse' {httpStatus} -> httpStatus) (\s@AssociateSourceNetworkStackResponse' {} a -> s {httpStatus = a} :: AssociateSourceNetworkStackResponse)

instance
  Prelude.NFData
    AssociateSourceNetworkStackResponse
  where
  rnf AssociateSourceNetworkStackResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
