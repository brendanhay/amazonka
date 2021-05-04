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
-- Module      : Network.AWS.SWF.UndeprecateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated domain. After a domain has been
-- undeprecated it can be used to create new workflow executions or
-- register new types.
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
module Network.AWS.SWF.UndeprecateDomain
  ( -- * Creating a Request
    UndeprecateDomain (..),
    newUndeprecateDomain,

    -- * Request Lenses
    undeprecateDomain_name,

    -- * Destructuring the Response
    UndeprecateDomainResponse (..),
    newUndeprecateDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newUndeprecateDomain' smart constructor.
data UndeprecateDomain = UndeprecateDomain'
  { -- | The name of the domain of the deprecated workflow type.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'undeprecateDomain_name' - The name of the domain of the deprecated workflow type.
newUndeprecateDomain ::
  -- | 'name'
  Prelude.Text ->
  UndeprecateDomain
newUndeprecateDomain pName_ =
  UndeprecateDomain' {name = pName_}

-- | The name of the domain of the deprecated workflow type.
undeprecateDomain_name :: Lens.Lens' UndeprecateDomain Prelude.Text
undeprecateDomain_name = Lens.lens (\UndeprecateDomain' {name} -> name) (\s@UndeprecateDomain' {} a -> s {name = a} :: UndeprecateDomain)

instance Prelude.AWSRequest UndeprecateDomain where
  type Rs UndeprecateDomain = UndeprecateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UndeprecateDomainResponse'

instance Prelude.Hashable UndeprecateDomain

instance Prelude.NFData UndeprecateDomain

instance Prelude.ToHeaders UndeprecateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.UndeprecateDomain" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UndeprecateDomain where
  toJSON UndeprecateDomain' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )

instance Prelude.ToPath UndeprecateDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UndeprecateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUndeprecateDomainResponse' smart constructor.
data UndeprecateDomainResponse = UndeprecateDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UndeprecateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUndeprecateDomainResponse ::
  UndeprecateDomainResponse
newUndeprecateDomainResponse =
  UndeprecateDomainResponse'

instance Prelude.NFData UndeprecateDomainResponse
