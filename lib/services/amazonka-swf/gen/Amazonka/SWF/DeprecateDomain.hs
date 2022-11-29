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
-- Module      : Amazonka.SWF.DeprecateDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified domain. After a domain has been deprecated it
-- cannot be used to create new workflow executions or register new types.
-- However, you can still use visibility actions on this domain.
-- Deprecating a domain also deprecates all activity and workflow types
-- registered in the domain. Executions that were started before the domain
-- was deprecated continues to run.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.DeprecateDomain
  ( -- * Creating a Request
    DeprecateDomain (..),
    newDeprecateDomain,

    -- * Request Lenses
    deprecateDomain_name,

    -- * Destructuring the Response
    DeprecateDomainResponse (..),
    newDeprecateDomainResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newDeprecateDomain' smart constructor.
data DeprecateDomain = DeprecateDomain'
  { -- | The name of the domain to deprecate.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deprecateDomain_name' - The name of the domain to deprecate.
newDeprecateDomain ::
  -- | 'name'
  Prelude.Text ->
  DeprecateDomain
newDeprecateDomain pName_ =
  DeprecateDomain' {name = pName_}

-- | The name of the domain to deprecate.
deprecateDomain_name :: Lens.Lens' DeprecateDomain Prelude.Text
deprecateDomain_name = Lens.lens (\DeprecateDomain' {name} -> name) (\s@DeprecateDomain' {} a -> s {name = a} :: DeprecateDomain)

instance Core.AWSRequest DeprecateDomain where
  type
    AWSResponse DeprecateDomain =
      DeprecateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeprecateDomainResponse'

instance Prelude.Hashable DeprecateDomain where
  hashWithSalt _salt DeprecateDomain' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeprecateDomain where
  rnf DeprecateDomain' {..} = Prelude.rnf name

instance Core.ToHeaders DeprecateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.DeprecateDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeprecateDomain where
  toJSON DeprecateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeprecateDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery DeprecateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeprecateDomainResponse' smart constructor.
data DeprecateDomainResponse = DeprecateDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeprecateDomainResponse ::
  DeprecateDomainResponse
newDeprecateDomainResponse = DeprecateDomainResponse'

instance Prelude.NFData DeprecateDomainResponse where
  rnf _ = ()
