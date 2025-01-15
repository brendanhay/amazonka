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
-- Module      : Amazonka.SWF.DescribeDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified domain, including description
-- and status.
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
module Amazonka.SWF.DescribeDomain
  ( -- * Creating a Request
    DescribeDomain (..),
    newDescribeDomain,

    -- * Request Lenses
    describeDomain_name,

    -- * Destructuring the Response
    DescribeDomainResponse (..),
    newDescribeDomainResponse,

    -- * Response Lenses
    describeDomainResponse_httpStatus,
    describeDomainResponse_domainInfo,
    describeDomainResponse_configuration,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newDescribeDomain' smart constructor.
data DescribeDomain = DescribeDomain'
  { -- | The name of the domain to describe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeDomain_name' - The name of the domain to describe.
newDescribeDomain ::
  -- | 'name'
  Prelude.Text ->
  DescribeDomain
newDescribeDomain pName_ =
  DescribeDomain' {name = pName_}

-- | The name of the domain to describe.
describeDomain_name :: Lens.Lens' DescribeDomain Prelude.Text
describeDomain_name = Lens.lens (\DescribeDomain' {name} -> name) (\s@DescribeDomain' {} a -> s {name = a} :: DescribeDomain)

instance Core.AWSRequest DescribeDomain where
  type
    AWSResponse DescribeDomain =
      DescribeDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "domainInfo")
            Prelude.<*> (x Data..:> "configuration")
      )

instance Prelude.Hashable DescribeDomain where
  hashWithSalt _salt DescribeDomain' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeDomain where
  rnf DescribeDomain' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.DescribeDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DescribeDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | Contains details of a domain.
--
-- /See:/ 'newDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The basic information about a domain, such as its name, status, and
    -- description.
    domainInfo :: DomainInfo,
    -- | The domain configuration. Currently, this includes only the domain\'s
    -- retention period.
    configuration :: DomainConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainInfo', 'describeDomainResponse_domainInfo' - The basic information about a domain, such as its name, status, and
-- description.
--
-- 'configuration', 'describeDomainResponse_configuration' - The domain configuration. Currently, this includes only the domain\'s
-- retention period.
newDescribeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainInfo'
  DomainInfo ->
  -- | 'configuration'
  DomainConfiguration ->
  DescribeDomainResponse
newDescribeDomainResponse
  pHttpStatus_
  pDomainInfo_
  pConfiguration_ =
    DescribeDomainResponse'
      { httpStatus = pHttpStatus_,
        domainInfo = pDomainInfo_,
        configuration = pConfiguration_
      }

-- | The response's http status code.
describeDomainResponse_httpStatus :: Lens.Lens' DescribeDomainResponse Prelude.Int
describeDomainResponse_httpStatus = Lens.lens (\DescribeDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainResponse' {} a -> s {httpStatus = a} :: DescribeDomainResponse)

-- | The basic information about a domain, such as its name, status, and
-- description.
describeDomainResponse_domainInfo :: Lens.Lens' DescribeDomainResponse DomainInfo
describeDomainResponse_domainInfo = Lens.lens (\DescribeDomainResponse' {domainInfo} -> domainInfo) (\s@DescribeDomainResponse' {} a -> s {domainInfo = a} :: DescribeDomainResponse)

-- | The domain configuration. Currently, this includes only the domain\'s
-- retention period.
describeDomainResponse_configuration :: Lens.Lens' DescribeDomainResponse DomainConfiguration
describeDomainResponse_configuration = Lens.lens (\DescribeDomainResponse' {configuration} -> configuration) (\s@DescribeDomainResponse' {} a -> s {configuration = a} :: DescribeDomainResponse)

instance Prelude.NFData DescribeDomainResponse where
  rnf DescribeDomainResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf domainInfo `Prelude.seq`
        Prelude.rnf configuration
